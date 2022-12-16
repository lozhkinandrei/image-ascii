{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import           AsciiConverter.Lib     ( Config (Config, imageColor, imageWidth),
                                          convertToAscii, resizeImage )
import           AsciiConverter.Read

import           Conduit                ( runConduitRes, sinkList, takeCE,
                                          (.|) )

import           Control.Exception      ( SomeException, try )
import           Control.Monad          ( unless )
import           Control.Monad.IO.Class

import           Data.Aeson             ( FromJSON, ToJSON )
import           Data.ByteString
import qualified Data.ByteString.Char8  ( readInt )
import           Data.Foldable

import           GHC.Generics

import           Graphics.Image         ( Image, RGB, VS )

import           Network.HTTP.Client    ( parseUrlThrow )
import           Network.HTTP.Simple
import           Network.HTTP.Types     ( hContentLength, status400 )

import           Web.Scotty

maxFileSize :: Int
maxFileSize = 1024

routes :: ScottyM ()
routes = do
    get "/hello" hello

hello :: ActionM ()
hello = do
    imageUrl <- param "imageUrl" `rescue` (\_ -> return "") :: ActionM String
    color <- param "color" `rescue` (\_ -> return True) :: ActionM Bool
    width <- param "width" `rescue` (\_ -> return 100) :: ActionM Int

    -- validate width
    case width of
        w | w < 10 -> status status400 >> (json $ BadRequestResponse { err = "width can't be less than 10" }) >> finish
        w | w > 300 -> status status400 >> (json $ BadRequestResponse { err = "width can't be greater than 300" }) >> finish
        _ -> return ()


    case imageUrl of
        "" -> status status400 >> (json $ BadRequestResponse { err = "imageUrl is required" }) >> finish
        _ -> do
            -- Parse url, throw an exception in case it's invalid
            req <- liftIO $ try (parseUrlThrow $ "HEAD " ++ imageUrl) :: ActionM (Either SomeException Request)

            -- Make a HEAD request first to check the content length
            case req of
                Left _ -> status status400 >> (json $ BadRequestResponse { err = "invalid imageUrl" }) >> finish
                Right request' -> do
                    res <- liftIO $ try (httpNoBody request') :: ActionM (Either SomeException (Response ()))

                    case res of
                        Left _ -> status status400 >> (json $ BadRequestResponse { err = "Couldn't get the response from provided imageUrl" }) >> finish
                        Right response -> do
                            let responseHeader = getResponseHeader hContentLength response
                            let contentLength = if Data.Foldable.length responseHeader == 0 then Nothing else Data.ByteString.Char8.readInt $ Prelude.head responseHeader

                            case contentLength of
                                Nothing -> return ()
                                Just (contentLength', _) ->
                                    unless (contentLength' `div` 1024 < maxFileSize)
                                        (status status400 >> (json $ BadRequestResponse { err = "File is too big" }) >> finish)

            -- make a GET request to get the actual file,
            -- note: get the data that doesn't exceed MAX_FILE_SIZE
            request' <- liftIO $ parseRequest imageUrl
            response <- liftIO $ runConduitRes $ httpSource request' getResponseBody
                        .| takeCE (maxFileSize * 1024)
                        .| sinkList

            let responseBody = Data.ByteString.concat response

            -- -- return 400 error if file size exceeds MAX_FILE_SIZE
            unless (Data.ByteString.length responseBody `div` 1024 < maxFileSize)
                 (status status400 >> (json $ BadRequestResponse { err = "File is too big" }) >> finish)

            -- -- config used for ascii converter
            let config = Config{imageWidth = width, imageColor = color}

            -- -- read image from byte string
            image <- liftIO $ AsciiConverter.Read.readImage imageUrl responseBody :: ActionM (Either String (Image VS RGB Double))

            case image of
                Left _ -> status status400 >> (json $ BadRequestResponse { err = "Couldn't read the image, either the file is corrupted or has the wrong format" }) >> finish
                Right img -> do
                    -- process the image
                    let resizedImg = resizeImage (imageWidth config) img
                    converted <- liftIO $ convertToAscii resizedImg config
                    (json $ AsciiResponse { res = converted }) >> finish


main :: IO ()
main = do
    putStrLn "Starting Server..."
    scotty 3000 routes



newtype BadRequestResponse
  = BadRequestResponse { err :: String }
  deriving (Generic, Show)
instance ToJSON BadRequestResponse

newtype AsciiResponse
  = AsciiResponse { res :: String }
  deriving (Generic, Show)
instance ToJSON AsciiResponse
