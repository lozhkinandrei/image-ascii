{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module AsciiConverter.Read where

import qualified Control.Monad                   as M ( foldM )

import           Data.ByteString                 ( ByteString )
import           Data.Char                       ( toLower )

import           Graphics.Image.Interface
import           Graphics.Image.Interface.Vector
import           Graphics.Image.IO.Formats

import           Prelude                         as P

import           System.FilePath                 ( takeExtension )


guessFormat :: (ImageFormat f, Enum f) => FilePath -> Maybe f
guessFormat path =
  headMaybe . dropWhile (not . isFormat e) . enumFrom . toEnum $ 0
  where e = P.map toLower . takeExtension $ path
        headMaybe ls = if null ls then Nothing else Just $ head ls


readImage :: forall arr cs e .
             (Array VS cs e, Array arr cs e,
              Readable (Image VS cs e) InputFormat) =>
             FilePath -> ByteString
          -> IO (Either String (Image arr cs e))
readImage path bs = do
  let maybeFormat = guessFormat path :: Maybe InputFormat
      formats = enumFrom . toEnum $ 0
      orderedFormats = maybe formats (\f -> f:P.filter (/=f) formats) maybeFormat
      reader :: Either String (Image VS cs e) -> InputFormat -> IO (Either String (Image VS cs e))
      reader (Left err) format =
        return $ either (Left . ((err++"\n")++)) Right (decode format bs)
      reader img         _     = return img
  imgE <- M.foldM reader (Left "") orderedFormats
  return $ fmap (exchange (undefined :: arr)) imgE
