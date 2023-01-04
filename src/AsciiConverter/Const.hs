module AsciiConverter.Const
    (
        asciiCharactersMap,
        maxBrightness,
        brightnessWeight
    ) where


asciiCharactersMap :: String
asciiCharactersMap = "`'.,^:;Il!i~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"

maxBrightness :: Int
maxBrightness = 255

brightnessWeight :: Double
brightnessWeight = fromIntegral (length asciiCharactersMap) / fromIntegral maxBrightness
