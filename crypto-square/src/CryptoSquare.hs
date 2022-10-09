module CryptoSquare (encode) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

encode :: String -> String
encode = T.unpack . T.unwords . getColumns . normalize . T.pack

-- We want an equation for the number of rows. We know the following:
--  * At least as many columns as rows.
--  * Difference between rows and columns is *at most* 1.
--
-- Thus if we let m be the number of rows, and n be the number of columns, then
-- either n = m or n = m + 1.
--
-- Let k be the number of characters in the sentence. Then we look at the
-- ceiling of the square root and the floor of the square root. These are our
-- two candidates for the number of rows.
getColumns :: Text -> [Text]
getColumns t = T.transpose $ map (T.justifyLeft n ' ') $ T.chunksOf n t
  where n = ceiling $ sqrt (fromIntegral $ T.length t :: Double)

normalize :: Text -> Text
normalize = T.toLower . T.filter isAlphaNum
