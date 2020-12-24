module Z.Data.ASCII where

import GHC.Word
import GHC.Exts

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
--
w2c :: Word8 -> Char
{-# INLINE w2c #-}
w2c (W8# w#) = C# (chr# (word2Int# w#))

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > @\\255@.
c2w :: Char -> Word8
{-# INLINE c2w #-}
c2w (C# c#) = W8# (int2Word# (ord# c#))

-- | @NUL <= w && w <= DEL@
isASCII :: Word8 -> Bool
{-# INLINE isASCII #-}
isASCII w = w <= DEL

-- | @A ~ Z@
isUpper :: Word8 -> Bool
{-# INLINE isUpper #-}
isUpper w = w - LETTER_A <= 25

-- | @a ~ z@
isLower :: Word8 -> Bool
{-# INLINE isLower #-}
isLower w =  w - LETTER_a <= 25

-- | @ISO-8859-1@ control letter.
isControl :: Word8 -> Bool
{-# INLINE isControl #-}
isControl w = w <= 0x1f || w - DEL <= 32

-- | @ISO-8859-1@ space letter.
isSpace :: Word8 -> Bool
{-# INLINE isSpace #-}
isSpace w = w == SPACE || w - TAB <= 4 || w == 0xa0

-- | @0 ~ 9@
isDigit :: Word8 -> Bool
{-# INLINE isDigit #-}
isDigit w = w - DIGIT_0 <= 9

-- | @0 ~ 7@
isOctDigit :: Word8 -> Bool
{-# INLINE isOctDigit #-}
isOctDigit w = w - DIGIT_0 <= 7

-- | @0 ~ 9, A ~ F, a ~ f@
isHexDigit :: Word8 -> Bool
{-# INLINE isHexDigit #-}
isHexDigit w = w - DIGIT_0 <= 9 || w - LETTER_A <= 5 || w - LETTER_a <= 5

----------------------------------------------------------------

-- | @\\NUL@
pattern NUL :: Word8
pattern NUL          = 0x00
-- | @\\t@
pattern TAB :: Word8
pattern TAB          = 0x09
-- | @\\n@
pattern NEWLINE :: Word8
pattern NEWLINE      = 0x0a
-- | @\\v@
pattern VERTICAL_TAB :: Word8
pattern VERTICAL_TAB = 0x0b
-- | @\\f@
pattern FORM_FEED :: Word8
pattern FORM_FEED    = 0x0c
-- | @\\r@
pattern CARRIAGE_RETURN :: Word8
pattern CARRIAGE_RETURN = 0x0d

-- | @\' \'@
pattern SPACE :: Word8
pattern SPACE        = 0x20
-- | @\!@
pattern EXCLAM :: Word8
pattern EXCLAM       = 0x21
-- | @\"@
pattern QUOTE_DOUBLE :: Word8
pattern QUOTE_DOUBLE = 0x22
-- | @\#@
pattern HASH :: Word8
pattern HASH         = 0x23
-- | @\#@
pattern NUMBER_SIGN :: Word8
pattern NUMBER_SIGN  = 0x23
-- | @\$@
pattern DOLLAR :: Word8
pattern DOLLAR       = 0x24
-- | @\%@
pattern PERCENT :: Word8
pattern PERCENT      = 0x25
-- | @\&@
pattern AMPERSAND :: Word8
pattern AMPERSAND    = 0x26
-- | @\&@
pattern AND :: Word8
pattern AND          = 0x26
-- | @\'@
pattern QUOTE_SINGLE :: Word8
pattern QUOTE_SINGLE = 0x27
-- | @(@
pattern PAREN_LEFT :: Word8
pattern PAREN_LEFT   = 0x28
-- | @)@
pattern PAREN_RIGHT :: Word8
pattern PAREN_RIGHT  = 0x29
-- | @*@
pattern ASTERISK :: Word8
pattern ASTERISK     = 0x2a
-- | @+@
pattern PLUS :: Word8
pattern PLUS         = 0x2b
-- | @,@
pattern COMMA :: Word8
pattern COMMA        = 0x2c
-- | @-@
pattern HYPHEN :: Word8
pattern HYPHEN       = 0x2d
-- | @-@
pattern MINUS :: Word8
pattern MINUS        = 0x2d
-- | @.@
pattern PERIOD :: Word8
pattern PERIOD       = 0x2e
-- | @.@
pattern DOT :: Word8
pattern DOT          = 0x2e
-- | @\/@
pattern SLASH :: Word8
pattern SLASH        = 0x2f

pattern DIGIT_0 :: Word8
pattern DIGIT_0      = 0x30
pattern DIGIT_1 :: Word8
pattern DIGIT_1      = 0x31
pattern DIGIT_2 :: Word8
pattern DIGIT_2      = 0x32
pattern DIGIT_3 :: Word8
pattern DIGIT_3      = 0x33
pattern DIGIT_4 :: Word8
pattern DIGIT_4      = 0x34
pattern DIGIT_5 :: Word8
pattern DIGIT_5      = 0x35
pattern DIGIT_6 :: Word8
pattern DIGIT_6      = 0x36
pattern DIGIT_7 :: Word8
pattern DIGIT_7      = 0x37
pattern DIGIT_8 :: Word8
pattern DIGIT_8      = 0x38
pattern DIGIT_9 :: Word8
pattern DIGIT_9      = 0x39

-- | @:@
pattern COLON :: Word8
pattern COLON        = 0x3a
-- | @;@
pattern SEMICOLON :: Word8
pattern SEMICOLON    = 0x3b
-- | @<@
pattern LESS :: Word8
pattern LESS         = 0x3c
-- | @<@
pattern ANGLE_LEFT :: Word8
pattern ANGLE_LEFT   = 0x3c
-- | @=@
pattern EQUAL :: Word8
pattern EQUAL        = 0x3d
-- | @>@
pattern GREATER :: Word8
pattern GREATER      = 0x3e
-- | @>@
pattern ANGLE_RIGHT :: Word8
pattern ANGLE_RIGHT  = 0x3e
-- | @\?@
pattern QUESTION :: Word8
pattern QUESTION     = 0x3f
-- | @\@@
pattern AT :: Word8
pattern AT           = 0x40

pattern LETTER_A :: Word8
pattern LETTER_A     = 0x41
pattern LETTER_B :: Word8
pattern LETTER_B     = 0x42
pattern LETTER_C :: Word8
pattern LETTER_C     = 0x43
pattern LETTER_D :: Word8
pattern LETTER_D     = 0x44
pattern LETTER_E :: Word8
pattern LETTER_E     = 0x45
pattern LETTER_F :: Word8
pattern LETTER_F     = 0x46
pattern LETTER_G :: Word8
pattern LETTER_G     = 0x47
pattern LETTER_H :: Word8
pattern LETTER_H     = 0x48
pattern LETTER_I :: Word8
pattern LETTER_I     = 0x49
pattern LETTER_J :: Word8
pattern LETTER_J     = 0x4a
pattern LETTER_K :: Word8
pattern LETTER_K     = 0x4b
pattern LETTER_L :: Word8
pattern LETTER_L     = 0x4c
pattern LETTER_M :: Word8
pattern LETTER_M     = 0x4d
pattern LETTER_N :: Word8
pattern LETTER_N     = 0x4e
pattern LETTER_O :: Word8
pattern LETTER_O     = 0x4f
pattern LETTER_P :: Word8
pattern LETTER_P     = 0x50
pattern LETTER_Q :: Word8
pattern LETTER_Q     = 0x51
pattern LETTER_R :: Word8
pattern LETTER_R     = 0x52
pattern LETTER_S :: Word8
pattern LETTER_S     = 0x53
pattern LETTER_T :: Word8
pattern LETTER_T     = 0x54
pattern LETTER_U :: Word8
pattern LETTER_U     = 0x55
pattern LETTER_V :: Word8
pattern LETTER_V     = 0x56
pattern LETTER_W :: Word8
pattern LETTER_W     = 0x57
pattern LETTER_X :: Word8
pattern LETTER_X     = 0x58
pattern LETTER_Y :: Word8
pattern LETTER_Y     = 0x59
pattern LETTER_Z :: Word8
pattern LETTER_Z     = 0x5a

-- | @[@
pattern BRACKET_LEFT :: Word8
pattern BRACKET_LEFT = 0x5b
-- | @[@
pattern SQUARE_LEFT :: Word8
pattern SQUARE_LEFT = 0x5b
-- | @\\@
pattern BACKSLASH :: Word8
pattern BACKSLASH    = 0x5c
-- | @]@
pattern BRACKET_RIGHT :: Word8
pattern BRACKET_RIGHT = 0x5d
-- | @]@
pattern SQUARE_RIGHT :: Word8
pattern SQUARE_RIGHT = 0x5d
-- | @^@
pattern CIRCUM :: Word8
pattern CIRCUM       = 0x5e
-- | @_@
pattern UNDERSCORE :: Word8
pattern UNDERSCORE   = 0x5f
-- | @`@
pattern GRAVE :: Word8
pattern GRAVE        = 0x60

pattern LETTER_a :: Word8
pattern LETTER_a     = 0x61
pattern LETTER_b :: Word8
pattern LETTER_b     = 0x62
pattern LETTER_c :: Word8
pattern LETTER_c     = 0x63
pattern LETTER_d :: Word8
pattern LETTER_d     = 0x64
pattern LETTER_e :: Word8
pattern LETTER_e     = 0x65
pattern LETTER_f :: Word8
pattern LETTER_f     = 0x66
pattern LETTER_g :: Word8
pattern LETTER_g     = 0x67
pattern LETTER_h :: Word8
pattern LETTER_h     = 0x68
pattern LETTER_i :: Word8
pattern LETTER_i     = 0x69
pattern LETTER_j :: Word8
pattern LETTER_j     = 0x6a
pattern LETTER_k :: Word8
pattern LETTER_k     = 0x6b
pattern LETTER_l :: Word8
pattern LETTER_l     = 0x6c
pattern LETTER_m :: Word8
pattern LETTER_m     = 0x6d
pattern LETTER_n :: Word8
pattern LETTER_n     = 0x6e
pattern LETTER_o :: Word8
pattern LETTER_o     = 0x6f
pattern LETTER_p :: Word8
pattern LETTER_p     = 0x70
pattern LETTER_q :: Word8
pattern LETTER_q     = 0x71
pattern LETTER_r :: Word8
pattern LETTER_r     = 0x72
pattern LETTER_s :: Word8
pattern LETTER_s     = 0x73
pattern LETTER_t :: Word8
pattern LETTER_t     = 0x74
pattern LETTER_u :: Word8
pattern LETTER_u     = 0x75
pattern LETTER_v :: Word8
pattern LETTER_v     = 0x76
pattern LETTER_w :: Word8
pattern LETTER_w     = 0x77
pattern LETTER_x :: Word8
pattern LETTER_x     = 0x78
pattern LETTER_y :: Word8
pattern LETTER_y     = 0x79
pattern LETTER_z :: Word8
pattern LETTER_z     = 0x7a

-- | @{@
pattern BRACE_LEFT :: Word8
pattern BRACE_LEFT   = 0x7b
-- | @{@
pattern CURLY_LEFT :: Word8
pattern CURLY_LEFT   = 0x7b
-- | @|@
pattern BAR :: Word8
pattern BAR          = 0x7c
-- | @|@
pattern OR :: Word8
pattern OR           = 0x7c
-- | @}@
pattern BRACE_RIGHT :: Word8
pattern BRACE_RIGHT  = 0x7d
-- | @}@
pattern CURLY_RIGHT :: Word8
pattern CURLY_RIGHT  = 0x7d
-- | @~@
pattern TILDE :: Word8
pattern TILDE        = 0x7e
-- | @\DEL@
pattern DEL :: Word8
pattern DEL          = 0x7f
