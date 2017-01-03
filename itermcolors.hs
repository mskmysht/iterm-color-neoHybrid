{-# LANGUAGE ExistentialQuantification #-}
import Data.List
import Data.Foldable

data Opt = forall a. Show a => O String a

data Tag = P String [Opt] [Tag]
         | C String String

instance Show Opt where
  show (O s a) = s ++ "=" ++ show a

instance Show Tag where
  show (P s os ts) = "<" ++ s ++ " " ++ show os ++ ": " ++ show ts ++ ">"
  show (C s a)     = "<" ++ s ++ ": " ++ a ++ ">"

plist :: [Opt] -> [Tag] -> Tag
plist = P "plist"

dict :: [Tag] -> Tag
dict = P "dict" []

key :: String -> Tag
key = C "key"

real :: Double -> Tag
real = C "real" . show

rgb2tag :: (Int, Int, Int) -> Tag
rgb2tag (r, g, b) =
  dict [ comp "Red"
        ,col r
        ,comp "Green"
        ,col g
        ,comp "Blue"
        ,col b]
  where
    comp s = key $ s ++ " Component"
    col c = real $ (realToFrac c) / 255.0


black = (0x28, 0x2a, 0x2e)
white = (0xc5, 0xc8, 0xc6)

colors = [black
         ,(0xA5, 0x42, 0x42)
         ,(0x8c, 0x94, 0x40)
         ,(0xde, 0x93, 0x5f)
         ,(0x5f, 0x81, 0x8f)
         ,(0x85, 0x67, 0x8f)
         ,(0x5e, 0x8d, 0x87)
         ,(0x70, 0x78, 0x80)
         ,(0x37, 0x3b, 0x41)
         ,(0xcc, 0x66, 0x66)
         ,(0xb5, 0xbd, 0x68)
         ,(0xf0, 0xc6, 0x74)
         ,(0x81, 0xa2, 0xbe)
         ,(0xb2, 0x94, 0xbb)
         ,(0x8a, 0xbe, 0xb7)
         ,white]

keyColor s = key $ s ++ " Color"

ansiColors :: [Tag]
ansiColors = concatMap (\(i, rgb) -> ansi i rgb) $ [0..] `zip` colors
  where
    ansi i rgb = [keyColor $ "Ansi " ++ show i, rgb2tag rgb]

background   = [keyColor "Background", rgb2tag black]
bold         = [keyColor "Bold", rgb2tag white]
cursor       = [keyColor "Corsor", rgb2tag white]
cursorText   = [keyColor "Corsor Text", rgb2tag black]
foreground   = [keyColor "Foreground", rgb2tag white]
selectedText = [keyColor "Selected Text", rgb2tag black]
selection    = [keyColor "Selection", rgb2tag white]

construct :: [Tag] -> [(Int, String)]
construct = foldr (construct' 0) []
  where
    construct' :: Int -> Tag -> [(Int, String)] -> [(Int, String)]
    construct' i t us = case t of
      P s os ts -> [(i, op $ f os s)]
        ++ (foldr (construct' (i + 1)) [(i, cl s)] ts)
        ++ us
      C s a     -> (i, oc s a) : us
      where
        op s = "<" ++ s ++ ">"
        cl s = "</" ++ s ++ ">"
        oc s a = op s ++ a ++ cl s
        f [] s = s
        f os s = intercalate " " $ s : map show os

main = do
  print $ P "foo" [] [C "hoge" "fuga"]
  print $ dict [key "fooo"]
  -- print $ rgb2tag 200 130 45
  -- for_ (colors `zip` [0..]) $ \((r, g, b), i) -> do
  --   putStrLn . show $ [key $ "Ansi " ++ show i ++ " Color", rgb2tag r g b]
  writeFile "neoHybrid.itermcolors"
    . (intercalate "\n")
    . map (\(i, s) -> (replicate i '\t') ++ s)
    . construct $ [
      plist [O "version" "1.0"] $ [
        dict $
          ansiColors
          ++ background
          ++ bold
          ++ cursor
          ++ cursorText
          ++ foreground
          ++ selectedText
          ++ selection]]
