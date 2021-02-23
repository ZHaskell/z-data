{-|
Module      : Z.Data.Text.Regex
Description : RE2 regex
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Binding to google's <https://github.com/google/re2 RE2>, microsoft did a nice job on RE2 regex syntaxs:
<https://docs.microsoft.com/en-us/deployedge/edge-learnmore-regex>. Note GHC string literals need @\\@ to
be escaped, e.g.

>>> match (regex "([a-z0-9_\\.-]+)@([\\da-z\\.-]+)\\.([a-z\\.]{2,6})") "please end email to hello@world.com, foo@bar.com"
>>> ("hello@world.com",[Just "hello",Just "world",Just "com"],", foo@bar.com")

-}

module Z.Data.Text.Regex
  ( -- * RE2 regex
    Regex, regex, RegexOpts(..), defaultRegexOpts, regexOpts
  , escape, regexCaptureNum, regexPattern
  , RegexException(..)
  -- * regex operations
  , test
  , match
  , replace
  , extract
  ) where

import Z.Foreign
import Control.Exception
import Data.Int
import Data.Word
import GHC.Stack
import GHC.Generics
import Foreign.ForeignPtr
import Foreign.Marshal.Utils            (fromBool)
import System.IO.Unsafe
import qualified Z.Data.Text.Base       as T
import qualified Z.Data.Text.Print      as T
import qualified Z.Data.Vector.Base     as V
import qualified Z.Data.Array           as A

-- | A compiled RE2 regex.
data Regex = Regex
    { regexPtr   :: {-# UNPACK #-} !(ForeignPtr Regex)
    , regexCaptureNum :: {-# UNPACK #-} !Int        -- ^ capturing group number(including @\\0@)
    , regexPattern :: T.Text                        -- ^ Get back regex's pattern.
    } deriving (Show, Generic)
      deriving anyclass T.Print

-- | RE2 Regex options.
--
-- The options are ('defaultRegexOpts' in parentheses):
--
-- @
--   posix_syntax     (false) restrict regexps to POSIX egrep syntax
--   longest_match    (false) search for longest match, not first match
--   log_errors       (true)  log syntax and execution errors to ERROR
--   max_mem          (8<<20)  approx. max memory footprint of RE2
--   literal          (false) interpret string as literal, not regexp
--   never_nl         (false) never match \\n, even if it is in regexp
--   dot_nl           (false) dot matches everything including new line
--   never_capture    (false) parse all parens as non-capturing
--   case_sensitive   (true)  match is case-sensitive (regexp can override
--                              with (?i) unless in posix_syntax mode)
-- @
--
-- The following options are only consulted when posix_syntax == true.
-- When posix_syntax == false, these features are always enabled and
-- cannot be turned off; to perform multi-line matching in that case,
-- begin the regexp with @(?m)@.
--
-- @
--   perl_classes     (false) allow Perl's \\d \\s \\w \\D \\S \\W
--   word_boundary    (false) allow Perl's \\b \\B (word boundary and not)
--   one_line         (false) ^ and $ only match beginning and end of text
-- @
--
data RegexOpts = RegexOpts
    { posix_syntax   :: Bool
    , longest_match  :: Bool

    , max_mem        :: {-# UNPACK #-} !Int64
    , literal        :: Bool
    , never_nl       :: Bool
    , dot_nl         :: Bool
    , never_capture  :: Bool
    , case_sensitive :: Bool


    , perl_classes   :: Bool
    , word_boundary  :: Bool
    , one_line       :: Bool
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass T.Print

-- | Default regex options, see 'RegexOpts'.
--
defaultRegexOpts :: RegexOpts
defaultRegexOpts = RegexOpts
    False False hs_re2_kDefaultMaxMem
    False False False False
    True False False False

-- | Exception thrown when using regex.
data RegexException = InvalidRegexPattern T.Text CallStack deriving Show
instance Exception RegexException

-- | Compile a regex pattern, throw 'InvalidRegexPattern' in case of illegal patterns.
--
regex :: HasCallStack => T.Text -> Regex
{-# NOINLINE regex #-}
regex t = unsafePerformIO $ do
    r <- withPrimVectorUnsafe (T.getUTF8Bytes t) hs_re2_compile_pattern_default
    ok <- hs_re2_ok r
    if ok /= 0
    then do
        p <- newForeignPtr p_hs_re2_delete_pattern r
        n <- hs_num_capture_groups r
        return (Regex p n t)
    else do
        hs_re2_delete_pattern r
        throwIO (InvalidRegexPattern t callStack)

-- | Compile a regex pattern withOptions, throw 'InvalidRegexPattern' in case of illegal patterns.
regexOpts :: HasCallStack => RegexOpts -> T.Text -> Regex
{-# NOINLINE regexOpts #-}
regexOpts RegexOpts{..} t = unsafePerformIO $ do
    r <- withPrimVectorUnsafe (T.getUTF8Bytes t) $ \ p o l ->
        hs_re2_compile_pattern p o l
            (fromBool posix_syntax  )
            (fromBool longest_match )
            max_mem
            (fromBool literal       )
            (fromBool never_nl      )
            (fromBool dot_nl        )
            (fromBool never_capture )
            (fromBool case_sensitive)
            (fromBool perl_classes  )
            (fromBool word_boundary )
            (fromBool one_line      )
    ok <- hs_re2_ok r
    if ok /= 0
    then do
        p <- newForeignPtr p_hs_re2_delete_pattern r
        n <- hs_num_capture_groups r
        return (Regex p n t)
    else do
        hs_re2_delete_pattern r
        throwIO (InvalidRegexPattern t callStack)

-- | Escape a piece of text literal so that it can be safely used in regex pattern.
--
-- >>> escape "(\\d+)"
-- >>> "\\(\\\\d\\+\\)"
--
escape :: T.Text -> T.Text
{-# INLINABLE escape #-}
escape t = T.Text . unsafePerformIO . fromStdString $
    withPrimVectorUnsafe (T.getUTF8Bytes t) hs_re2_quote_meta

-- | Check if text matched regex pattern.
test :: Regex -> T.Text -> Bool
{-# INLINABLE test #-}
test (Regex fp _ _) (T.Text bs) = unsafePerformIO $ do
    withForeignPtr fp $ \ p ->
        withPrimVectorUnsafe bs $ \ ba# s l -> do
            r <- hs_re2_test p ba# s l
            return $! r /= 0

-- | Check if text matched regex pattern,
-- if so return matched part, all capturing groups(from @\\1@) and the text after matched part.
--
-- @Nothing@ indicate a non-matching capturing group, e.g.
--
-- >>> match (regex "(foo)|(bar)baz") "barbazbla"
-- >>> ("barbaz",[Nothing,Just "bar"], "bla")
--
match :: Regex -> T.Text -> (T.Text, [Maybe T.Text], T.Text)
{-# INLINABLE match #-}
match (Regex fp n _) t@(T.Text bs@(V.PrimVector ba _ _)) = unsafePerformIO $ do
    withForeignPtr fp $ \ p ->
        withPrimVectorUnsafe bs $ \ ba# s l -> do
            (starts, (lens, r)) <- allocPrimArrayUnsafe n $ \ p_starts ->
                allocPrimArrayUnsafe n $ \ p_ends ->
                    hs_re2_match p ba# s l n p_starts p_ends
            if r == 0
            then return (T.empty, [], t)
            else do
                let !s0 = A.indexArr starts 0
                    !l0 = A.indexArr lens 0
                    caps = (map (\ !i ->
                        let !s' = A.indexArr starts i
                            !l' = A.indexArr lens i
                        in if l' == -1
                            then Nothing
                            else (Just (T.Text (V.PrimVector ba s' l')))) [1..n-1])
                return (T.Text (V.PrimVector ba s0 l0)
                       , caps
                       , T.Text (V.PrimVector ba (s0+l0) (s+l-s0-l0)))

-- | Replace matched part in input with a rewrite pattern.
-- If no matched part found, return the original input.
--
-- >>> replace (regex "red") False "A red fox with red fur" "yellow"
-- >>> "A yellow fox with red fur"
-- >>> replace (regex "red") True  "A red fox with red fur" "yellow"
-- >>> "A yellow fox with yellow fur"
--
replace :: Regex
        -> Bool     -- ^ globally replace?
        -> T.Text   -- ^ input
        -> T.Text   -- ^ rewrite
        -> T.Text
{-# INLINABLE replace #-}
replace (Regex fp _ _) g inp rew = T.Text . unsafePerformIO $ do
    withForeignPtr fp $ \ p ->
        withPrimVectorUnsafe (T.getUTF8Bytes inp) $ \ inpp inpoff inplen ->
            withPrimVectorUnsafe (T.getUTF8Bytes rew) $ \ rewp rewoff rewlen ->
                fromStdString ((if g then hs_re2_replace_g else hs_re2_replace)
                    p inpp inpoff inplen rewp rewoff rewlen)

-- | Extract capturing group to an extract pattern.
-- If no matched capturing group found, return an empty string.
--
-- >>> extract (regex "(\\d{4})-(\\d{2})-(\\d{2})") "Today is 2020-12-15" "month: \\2, date: \\3"
-- >>> "month: 12, date: 15"
--
extract :: Regex
        -> T.Text   -- ^ input
        -> T.Text   -- ^ extract
        -> T.Text
{-# INLINABLE extract #-}
extract (Regex fp _ _) inp rew = T.Text . unsafePerformIO $ do
    withForeignPtr fp $ \ p ->
        withPrimVectorUnsafe (T.getUTF8Bytes inp) $ \ inpp inpoff inplen ->
            withPrimVectorUnsafe (T.getUTF8Bytes rew) $ \ rewp rewoff rewlen ->
                fromStdString (hs_re2_extract p inpp inpoff inplen rewp rewoff rewlen)

--------------------------------------------------------------------------------

foreign import ccall unsafe hs_re2_compile_pattern_default :: BA# Word8 -> Int -> Int -> IO (Ptr Regex)
foreign import ccall unsafe hs_re2_compile_pattern
    :: BA# Word8 -> Int -> Int
    -> CBool -- ^ posix_syntax
    -> CBool -- ^ longest_match
    -> Int64 -- ^ max_mem
    -> CBool -- ^ literal
    -> CBool -- ^ never_nl
    -> CBool -- ^ dot_nl
    -> CBool -- ^ never_capture
    -> CBool -- ^ case_sensitive
    -> CBool -- ^ perl_classes
    -> CBool -- ^ word_boundary
    -> CBool -- ^ one_line
    -> IO (Ptr Regex)

foreign import ccall unsafe "&hs_re2_delete_pattern" p_hs_re2_delete_pattern :: FinalizerPtr Regex
foreign import ccall unsafe hs_re2_delete_pattern :: Ptr Regex -> IO ()

foreign import ccall unsafe hs_re2_ok :: Ptr Regex -> IO CInt
foreign import ccall unsafe hs_num_capture_groups :: Ptr Regex -> IO Int

foreign import ccall unsafe hs_re2_quote_meta :: BA# Word8 -> Int -> Int -> IO (Ptr StdString)

foreign import ccall unsafe hs_re2_match :: Ptr Regex
                                         -> BA# Word8   -- ^ input
                                         -> Int        -- ^ input offest
                                         -> Int        -- ^ input length
                                         -> Int         -- ^ capture num
                                         -> MBA# Int    -- ^ capture starts
                                         -> MBA# Int    -- ^ capture lens
                                         -> IO CInt        -- ^ 0 for failure, 1 for success

foreign import ccall unsafe hs_re2_test :: Ptr Regex
                                          -> BA# Word8   -- ^ input
                                          -> Int        -- ^ input offest
                                          -> Int        -- ^ input length
                                          -> IO CInt        -- ^ 0 for failure, 1 for success

foreign import ccall unsafe hs_re2_replace :: Ptr Regex
                                           -> BA# Word8   -- ^ input
                                           -> Int        -- ^ input offest
                                           -> Int        -- ^ input length
                                           -> BA# Word8   -- ^ rewrite
                                           -> Int        -- ^ rewrite offest
                                           -> Int        -- ^ rewrite length
                                           -> IO (Ptr StdString) -- ^ NULL for failure

foreign import ccall unsafe hs_re2_replace_g :: Ptr Regex
                                             -> BA# Word8   -- ^ input
                                             -> Int        -- ^ input offest
                                             -> Int        -- ^ input length
                                             -> BA# Word8   -- ^ rewrite
                                             -> Int        -- ^ rewrite offest
                                             -> Int        -- ^ rewrite length
                                             -> IO (Ptr StdString) -- ^ NULL for failure

foreign import ccall unsafe hs_re2_extract :: Ptr Regex
                                           -> BA# Word8   -- ^ input
                                           -> Int        -- ^ input offest
                                           -> Int        -- ^ input length
                                           -> BA# Word8   -- ^ rewrite
                                           -> Int        -- ^ rewrite offest
                                           -> Int        -- ^ rewrite length
                                           -> IO (Ptr StdString) -- ^ NULL for failure

foreign import ccall unsafe hs_re2_kDefaultMaxMem :: Int64
