module AsciiConverter.Lib
    (
        Config (..),
        resizeImage,
        convertToAscii,
    ) where

import           AsciiConverter.Const     ( asciiCharactersMap,
                                            brightnessWeight )

import           Data.List                ( map )
import           Data.Vector.Storable     ( toList )

import           Graphics.Image           as I
import           Graphics.Image.Interface ( Array (toVector) )

import           Prelude




data Config = Config
  { imageWidth :: Int
  , imageColor :: Bool
  }


resizeImage :: Array arr cs e => Int -> Image arr cs e -> Image arr cs e
resizeImage width img = scale Bilinear Edge (scaleFactor, scaleFactor) img
    where
        currentWidth = I.cols img
        scaleFactor = fromIntegral width / fromIntegral currentWidth :: Double

replacePixel :: Pixel RGB Double -> String
replacePixel (PixelRGB r g b) = character
    where
        red =  r * 255 :: Double
        green =  g * 255 :: Double
        blue =  b * 255 :: Double
        i = floor $ (0.2126 * red + 0.7152 * green + 0.0722 * blue) * brightnessWeight :: Int
        character = [asciiCharactersMap !! i]

replacePixelColor :: Pixel RGB Double -> String
replacePixelColor (PixelRGB r g b) = colorCharacter
    where
        red =  r * 255 :: Double
        green =  g * 255 :: Double
        blue =  b * 255 :: Double
        color = "rgb(" ++ show (round red :: Int) ++ " " ++ show (round green :: Int) ++ " " ++ show (round blue :: Int) ++ ")" :: String
        i = floor $ (0.2126 * red + 0.7152 * green + 0.0722 * blue) * brightnessWeight :: Int
        character = [asciiCharactersMap !! i]
        colorCharacter = "<span style='color:" ++ color ++ "'>" ++ character ++ "</span>"

convertToAscii :: Image VS RGB Double -> Config -> IO String
convertToAscii img config = do
    let pixelsVector = toVector img
    let pixelsList = toList pixelsVector :: [Pixel RGB Double]
    let converted = Data.List.map convert' pixelsList
    let withLineBreaks = insertAtN (imageWidth config) "<br>" converted
    return $ concat withLineBreaks

    where convert' = if imageColor config then replacePixelColor else replacePixel


insertAtN :: Int -> a -> [a] -> [a]
insertAtN 0 _ xs = xs
insertAtN _ _ [] = []
insertAtN n y xs
 | length xs < n = xs
 | otherwise = take n xs ++ [y] ++ insertAtN n y (drop n xs)
