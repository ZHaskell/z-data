{-|
Module      : Z.Data.Text.UTF8Rewind
Description : Errno provided by libuv
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

INTERNAL MODULE, provides utf8rewind constants

-}

module Z.Data.Text.UTF8Rewind where

import Foreign.C.Types
import GHC.Generics

#include "utf8rewind.h"

-- | Locale for case mapping.
type Locale = CSize 

pattern LocaleDefault              :: Locale 
pattern LocaleDefault               = #const UTF8_LOCALE_DEFAULT
pattern LocaleLithuanian           :: Locale
pattern LocaleLithuanian            = #const UTF8_LOCALE_LITHUANIAN
pattern LocaleTurkishAndAzeriLatin :: Locale
pattern LocaleTurkishAndAzeriLatin  = #const UTF8_LOCALE_TURKISH_AND_AZERI_LATIN

-- | Get environment locale
foreign import ccall unsafe "utf8envlocale" envLocale :: IO Locale

{-|
These are the Unicode Normalization Forms:

@
Form                         | Description
---------------------------- | ---------------------------------------------
Normalization Form D (NFD)   | Canonical decomposition
Normalization Form C (NFC)   | Canonical decomposition, followed by canonical composition
Normalization Form KD (NFKD) | Compatibility decomposition
Normalization Form KC (NFKC) | Compatibility decomposition, followed by canonical composition
@ 
-}
data NormalizeMode = NFC | NFKC | NFD | NFKD deriving (Show, Eq, Ord, Generic)

normalizeModeToFlag :: NormalizeMode -> CSize
normalizeModeToFlag NFC  = #{const UTF8_NORMALIZE_COMPOSE}
normalizeModeToFlag NFKC = #{const UTF8_NORMALIZE_COMPOSE} + #{const UTF8_NORMALIZE_COMPATIBILITY}
normalizeModeToFlag NFD  = #{const UTF8_NORMALIZE_DECOMPOSE}
normalizeModeToFlag NFKD = #{const UTF8_NORMALIZE_DECOMPOSE} + #{const UTF8_NORMALIZE_COMPATIBILITY}

data NormalizationResult = NormalizedYes | NormalizedMaybe | NormalizedNo deriving (Show, Eq, Ord, Generic)

toNormalizationResult :: Int -> NormalizationResult
toNormalizationResult #{const UTF8_NORMALIZATION_RESULT_YES} = NormalizedYes
toNormalizationResult #{const UTF8_NORMALIZATION_RESULT_MAYBE} = NormalizedMaybe
toNormalizationResult #{const UTF8_NORMALIZATION_RESULT_NO} = NormalizedNo


-- | Unicode categories.
--
-- See 'Z.Data.Text.Base.isCategory', you can combine categories with bitwise or.
type Category = CSize 

pattern CategoryLetterUppercase       :: Category
pattern CategoryLetterLowercase       :: Category
pattern CategoryLetterTitlecase       :: Category
pattern CategoryLetterOther           :: Category
pattern CategoryLetter                :: Category
pattern CategoryCaseMapped            :: Category
pattern CategoryLetterUppercase        = #const UTF8_CATEGORY_LETTER_UPPERCASE
pattern CategoryLetterLowercase        = #const UTF8_CATEGORY_LETTER_LOWERCASE
pattern CategoryLetterTitlecase        = #const UTF8_CATEGORY_LETTER_TITLECASE
pattern CategoryLetterOther            = #const UTF8_CATEGORY_LETTER_OTHER
pattern CategoryLetter                 = #const UTF8_CATEGORY_LETTER
pattern CategoryCaseMapped             = #const UTF8_CATEGORY_CASE_MAPPED

pattern CategoryMarkNonSpacing        :: Category  
pattern CategoryMarkSpacing           :: Category  
pattern CategoryMarkEnclosing         :: Category  
pattern CategoryMark                  :: Category  
pattern CategoryMarkNonSpacing         = #const UTF8_CATEGORY_MARK_NON_SPACING
pattern CategoryMarkSpacing            = #const UTF8_CATEGORY_MARK_SPACING
pattern CategoryMarkEnclosing          = #const UTF8_CATEGORY_MARK_ENCLOSING
pattern CategoryMark                   = #const UTF8_CATEGORY_MARK

pattern CategoryNumberDecimal         :: Category 
pattern CategoryNumberLetter          :: Category 
pattern CategoryNumberOther           :: Category 
pattern CategoryNumber                :: Category 
pattern CategoryNumberDecimal          = #const UTF8_CATEGORY_NUMBER_DECIMAL
pattern CategoryNumberLetter           = #const UTF8_CATEGORY_NUMBER_LETTER
pattern CategoryNumberOther            = #const UTF8_CATEGORY_NUMBER_OTHER
pattern CategoryNumber                 = #const UTF8_CATEGORY_NUMBER

pattern CategoryPunctuationConnector  :: Category
pattern CategoryPunctuationDash       :: Category
pattern CategoryPunctuationOpen       :: Category
pattern CategoryPunctuationClose      :: Category
pattern CategoryPunctuationInitial    :: Category
pattern CategoryPunctuationFinal      :: Category
pattern CategoryPunctuationOther      :: Category
pattern CategoryPunctuation           :: Category
pattern CategoryPunctuationConnector   = #const UTF8_CATEGORY_PUNCTUATION_CONNECTOR
pattern CategoryPunctuationDash        = #const UTF8_CATEGORY_PUNCTUATION_DASH
pattern CategoryPunctuationOpen        = #const UTF8_CATEGORY_PUNCTUATION_OPEN
pattern CategoryPunctuationClose       = #const UTF8_CATEGORY_PUNCTUATION_CLOSE
pattern CategoryPunctuationInitial     = #const UTF8_CATEGORY_PUNCTUATION_INITIAL
pattern CategoryPunctuationFinal       = #const UTF8_CATEGORY_PUNCTUATION_FINAL
pattern CategoryPunctuationOther       = #const UTF8_CATEGORY_PUNCTUATION_OTHER
pattern CategoryPunctuation            = #const UTF8_CATEGORY_PUNCTUATION
pattern CategorySymbolMath            :: Category 
pattern CategorySymbolCurrency        :: Category 
pattern CategorySymbolModifier        :: Category 
pattern CategorySymbolOther           :: Category 
pattern CategorySymbol                :: Category 
pattern CategorySymbolMath             = #const UTF8_CATEGORY_SYMBOL_MATH
pattern CategorySymbolCurrency         = #const UTF8_CATEGORY_SYMBOL_CURRENCY
pattern CategorySymbolModifier         = #const UTF8_CATEGORY_SYMBOL_MODIFIER
pattern CategorySymbolOther            = #const UTF8_CATEGORY_SYMBOL_OTHER
pattern CategorySymbol                 = #const UTF8_CATEGORY_SYMBOL

pattern CategorySeparatorSpace        :: Category 
pattern CategorySeparatorLine         :: Category 
pattern CategorySeparatorParagraph    :: Category 
pattern CategorySeparator             :: Category 
pattern CategoryControl               :: Category 
pattern CategoryFormat                :: Category 
pattern CategorySurrogate             :: Category 
pattern CategoryPrivateUse            :: Category 
pattern CategoryUnassigned            :: Category 
pattern CategoryCompatibility         :: Category 
pattern CategoryIgnoreGraphemeCluster :: Category 
pattern CategoryIscntrl               :: Category 
pattern CategorySeparatorSpace         = #const UTF8_CATEGORY_SEPARATOR_SPACE
pattern CategorySeparatorLine          = #const UTF8_CATEGORY_SEPARATOR_LINE
pattern CategorySeparatorParagraph     = #const UTF8_CATEGORY_SEPARATOR_PARAGRAPH
pattern CategorySeparator              = #const UTF8_CATEGORY_SEPARATOR
pattern CategoryControl                = #const UTF8_CATEGORY_CONTROL
pattern CategoryFormat                 = #const UTF8_CATEGORY_FORMAT
pattern CategorySurrogate              = #const UTF8_CATEGORY_SURROGATE
pattern CategoryPrivateUse             = #const UTF8_CATEGORY_PRIVATE_USE
pattern CategoryUnassigned             = #const UTF8_CATEGORY_UNASSIGNED
pattern CategoryCompatibility          = #const UTF8_CATEGORY_COMPATIBILITY
pattern CategoryIgnoreGraphemeCluster  = #const UTF8_CATEGORY_IGNORE_GRAPHEME_CLUSTER
pattern CategoryIscntrl                = #const UTF8_CATEGORY_ISCNTRL

pattern CategoryIsprint               :: Category
pattern CategoryIsspace               :: Category
pattern CategoryIsblank               :: Category
pattern CategoryIsgraph               :: Category
pattern CategoryIspunct               :: Category
pattern CategoryIsalnum               :: Category
pattern CategoryIsalpha               :: Category
pattern CategoryIsupper               :: Category
pattern CategoryIslower               :: Category
pattern CategoryIsdigit               :: Category
pattern CategoryIsxdigit              :: Category
pattern CategoryIsprint                = #const UTF8_CATEGORY_ISPRINT
pattern CategoryIsspace                = #const UTF8_CATEGORY_ISSPACE
pattern CategoryIsblank                = #const UTF8_CATEGORY_ISBLANK
pattern CategoryIsgraph                = #const UTF8_CATEGORY_ISGRAPH
pattern CategoryIspunct                = #const UTF8_CATEGORY_ISPUNCT
pattern CategoryIsalnum                = #const UTF8_CATEGORY_ISALNUM
pattern CategoryIsalpha                = #const UTF8_CATEGORY_ISALPHA
pattern CategoryIsupper                = #const UTF8_CATEGORY_ISUPPER
pattern CategoryIslower                = #const UTF8_CATEGORY_ISLOWER
pattern CategoryIsdigit                = #const UTF8_CATEGORY_ISDIGIT
pattern CategoryIsxdigit               = #const UTF8_CATEGORY_ISXDIGIT                   
