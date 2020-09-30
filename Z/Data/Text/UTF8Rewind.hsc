{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

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

import Data.Bits
import Foreign.C.Types
import GHC.Generics

#include "utf8rewind.h"

-- | Locale for case mapping.
newtype Locale = Locale CSize deriving (Show, Eq, Ord, Generic)

pattern LocaleDefault              :: Locale 
pattern LocaleDefault               = Locale (#const UTF8_LOCALE_DEFAULT)
pattern LocaleLithuanian           :: Locale
pattern LocaleLithuanian            = Locale (#const UTF8_LOCALE_LITHUANIAN)
pattern LocaleTurkishAndAzeriLatin :: Locale
pattern LocaleTurkishAndAzeriLatin  = Locale (#const UTF8_LOCALE_TURKISH_AND_AZERI_LATIN)

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
newtype Category = Category CSize deriving (Show, Eq, Ord, Bits, FiniteBits, Generic)

pattern CategoryLetterUppercase       :: Category
pattern CategoryLetterLowercase       :: Category
pattern CategoryLetterTitlecase       :: Category
pattern CategoryLetterOther           :: Category
pattern CategoryLetter                :: Category
pattern CategoryCaseMapped            :: Category
pattern CategoryLetterUppercase        = Category (#const UTF8_CATEGORY_LETTER_UPPERCASE)
pattern CategoryLetterLowercase        = Category (#const UTF8_CATEGORY_LETTER_LOWERCASE)
pattern CategoryLetterTitlecase        = Category (#const UTF8_CATEGORY_LETTER_TITLECASE)
pattern CategoryLetterOther            = Category (#const UTF8_CATEGORY_LETTER_OTHER)
pattern CategoryLetter                 = Category (#const UTF8_CATEGORY_LETTER)
pattern CategoryCaseMapped             = Category (#const UTF8_CATEGORY_CASE_MAPPED)

pattern CategoryMarkNonSpacing        :: Category  
pattern CategoryMarkSpacing           :: Category  
pattern CategoryMarkEnclosing         :: Category  
pattern CategoryMark                  :: Category  
pattern CategoryMarkNonSpacing         = Category (#const UTF8_CATEGORY_MARK_NON_SPACING)
pattern CategoryMarkSpacing            = Category (#const UTF8_CATEGORY_MARK_SPACING)
pattern CategoryMarkEnclosing          = Category (#const UTF8_CATEGORY_MARK_ENCLOSING)
pattern CategoryMark                   = Category (#const UTF8_CATEGORY_MARK)

pattern CategoryNumberDecimal         :: Category 
pattern CategoryNumberLetter          :: Category 
pattern CategoryNumberOther           :: Category 
pattern CategoryNumber                :: Category 
pattern CategoryNumberDecimal          = Category (#const UTF8_CATEGORY_NUMBER_DECIMAL)
pattern CategoryNumberLetter           = Category (#const UTF8_CATEGORY_NUMBER_LETTER)
pattern CategoryNumberOther            = Category (#const UTF8_CATEGORY_NUMBER_OTHER)
pattern CategoryNumber                 = Category (#const UTF8_CATEGORY_NUMBER)

pattern CategoryPunctuationConnector  :: Category
pattern CategoryPunctuationDash       :: Category
pattern CategoryPunctuationOpen       :: Category
pattern CategoryPunctuationClose      :: Category
pattern CategoryPunctuationInitial    :: Category
pattern CategoryPunctuationFinal      :: Category
pattern CategoryPunctuationOther      :: Category
pattern CategoryPunctuation           :: Category
pattern CategoryPunctuationConnector   = Category (#const UTF8_CATEGORY_PUNCTUATION_CONNECTOR)
pattern CategoryPunctuationDash        = Category (#const UTF8_CATEGORY_PUNCTUATION_DASH)
pattern CategoryPunctuationOpen        = Category (#const UTF8_CATEGORY_PUNCTUATION_OPEN)
pattern CategoryPunctuationClose       = Category (#const UTF8_CATEGORY_PUNCTUATION_CLOSE)
pattern CategoryPunctuationInitial     = Category (#const UTF8_CATEGORY_PUNCTUATION_INITIAL)
pattern CategoryPunctuationFinal       = Category (#const UTF8_CATEGORY_PUNCTUATION_FINAL)
pattern CategoryPunctuationOther       = Category (#const UTF8_CATEGORY_PUNCTUATION_OTHER)
pattern CategoryPunctuation            = Category (#const UTF8_CATEGORY_PUNCTUATION)
pattern CategorySymbolMath            :: Category 
pattern CategorySymbolCurrency        :: Category 
pattern CategorySymbolModifier        :: Category 
pattern CategorySymbolOther           :: Category 
pattern CategorySymbol                :: Category 
pattern CategorySymbolMath             = Category (#const UTF8_CATEGORY_SYMBOL_MATH)
pattern CategorySymbolCurrency         = Category (#const UTF8_CATEGORY_SYMBOL_CURRENCY)
pattern CategorySymbolModifier         = Category (#const UTF8_CATEGORY_SYMBOL_MODIFIER)
pattern CategorySymbolOther            = Category (#const UTF8_CATEGORY_SYMBOL_OTHER)
pattern CategorySymbol                 = Category (#const UTF8_CATEGORY_SYMBOL)

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
pattern CategorySeparatorSpace         = Category (#const UTF8_CATEGORY_SEPARATOR_SPACE)
pattern CategorySeparatorLine          = Category (#const UTF8_CATEGORY_SEPARATOR_LINE)
pattern CategorySeparatorParagraph     = Category (#const UTF8_CATEGORY_SEPARATOR_PARAGRAPH)
pattern CategorySeparator              = Category (#const UTF8_CATEGORY_SEPARATOR)
pattern CategoryControl                = Category (#const UTF8_CATEGORY_CONTROL)
pattern CategoryFormat                 = Category (#const UTF8_CATEGORY_FORMAT)
pattern CategorySurrogate              = Category (#const UTF8_CATEGORY_SURROGATE)
pattern CategoryPrivateUse             = Category (#const UTF8_CATEGORY_PRIVATE_USE)
pattern CategoryUnassigned             = Category (#const UTF8_CATEGORY_UNASSIGNED)
pattern CategoryCompatibility          = Category (#const UTF8_CATEGORY_COMPATIBILITY)
pattern CategoryIgnoreGraphemeCluster  = Category (#const UTF8_CATEGORY_IGNORE_GRAPHEME_CLUSTER)
pattern CategoryIscntrl                = Category (#const UTF8_CATEGORY_ISCNTRL)

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
pattern CategoryIsprint                = Category (#const UTF8_CATEGORY_ISPRINT)
pattern CategoryIsspace                = Category (#const UTF8_CATEGORY_ISSPACE)
pattern CategoryIsblank                = Category (#const UTF8_CATEGORY_ISBLANK)
pattern CategoryIsgraph                = Category (#const UTF8_CATEGORY_ISGRAPH)
pattern CategoryIspunct                = Category (#const UTF8_CATEGORY_ISPUNCT)
pattern CategoryIsalnum                = Category (#const UTF8_CATEGORY_ISALNUM)
pattern CategoryIsalpha                = Category (#const UTF8_CATEGORY_ISALPHA)
pattern CategoryIsupper                = Category (#const UTF8_CATEGORY_ISUPPER)
pattern CategoryIslower                = Category (#const UTF8_CATEGORY_ISLOWER)
pattern CategoryIsdigit                = Category (#const UTF8_CATEGORY_ISDIGIT)
pattern CategoryIsxdigit               = Category (#const UTF8_CATEGORY_ISXDIGIT)                   
