{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Color where

-- Typeclass for things that can be used as colors.
class ColorString color where
  toString :: color -> String

-- Typeclass for things that can have colors applied to them.
-- Syntax: colorable % color = colored
class ColorFormattable a where
  (%) :: ColorString color => a -> color -> a

-- Allowing `Color` to be used as colors.
instance ColorString Color where
  toString :: Color -> String
  toString = show

-- Allowing lists of `Color` to be used as a valid color.
-- A list of colors applies each color in order of the list.
-- E.g. "Text" % [Red, Blue] applies Red then Blue, so the text becomes Blue.
instance ColorString [Color] where
  toString :: [Color] -> String
  toString = concatMap show

-- Allow colors to be specified by their color code
instance ColorString Int where
  toString :: Int -> String
  toString = colorCode

-- Allow colors to be specified by their color code
-- Example: "Red Text" % 91
instance ColorString Integer where
  toString :: Integer -> String
  toString = colorCode

instance ColorString [Int] where
  toString :: [Int] -> String
  toString = concatMap colorCode

instance ColorString [Integer] where
  toString :: [Integer] -> String
  toString = concatMap colorCode

-- Allow strings to have colors applied to them with the (%) operator.
instance ColorFormattable String where
  (%) :: ColorString color => String -> color -> String
  text % code = toString code ++ text ++ show Reset

-- Allows a list of strings to have colors applied to them.
-- The color given is applied to all elements of the list.
instance ColorFormattable [String] where
  (%) :: ColorString color => [String] -> color -> [String]
  text % code = map (\x -> toString code ++ x ++ show Reset) text

colorCode :: Show a => a -> String
colorCode x = "\ESC[" ++ show x ++ "m"

data Color = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | Gray
  | LightRed
  | LightGreen
  | LightYellow
  | LightBlue
  | LightMagenta
  | LightCyan
  | BlackBack
  | RedBack
  | GreenBack
  | YellowBack
  | BlueBack
  | MagentaBack
  | CyanBack
  | GrayBack
  | White
  | Reset
  | Bold
  | Dim
  | Italic
  | Underline
  | Blink
  | Invert

instance Show Color where
  show :: Color -> String
  show Black = colorCode 30
  show Red = colorCode 31
  show Green = colorCode 32
  show Yellow = colorCode 33
  show Blue = colorCode 34
  show Magenta = colorCode 35
  show Cyan = colorCode 36
  show Gray = colorCode 90
  show LightRed = colorCode 91
  show LightGreen = colorCode 92
  show LightYellow = colorCode 93
  show LightBlue = colorCode 94
  show LightMagenta = colorCode 95
  show LightCyan = colorCode 96
  show BlackBack = colorCode 40
  show RedBack = colorCode 41
  show GreenBack = colorCode 42
  show YellowBack = colorCode 43
  show BlueBack = colorCode 44
  show MagentaBack = colorCode 45
  show CyanBack = colorCode 46
  show GrayBack = colorCode 40
  show White = colorCode 97
  show Reset = colorCode 0
  show Bold = colorCode 1
  show Dim = colorCode 2
  show Italic = colorCode 3
  show Underline = colorCode 4
  show Blink = colorCode 5
  show Invert = colorCode 7
