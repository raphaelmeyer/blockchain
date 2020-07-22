module Blockchain where

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Enc

-- import           Text.Printf                    ( printf )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )

newtype Blockchain = Blockchain [Block]

data Block = Block {
  blockHash :: Hash,
  blockContent :: Text.Text,
  blockTimestamp :: Timestamp,
  blockPrevious :: Hash
}

type Hash = BS.ByteString
type Timestamp = String

newBlockchain :: IO Blockchain
newBlockchain = do
  timestamp <- getTimestamp
  return (Blockchain [newBlock (Text.pack "Genesis Block") timestamp BS.empty])

newBlock :: Text.Text -> Timestamp -> Hash -> Block
newBlock content timestamp previous = Block { blockHash      = hash
                                            , blockContent   = content
                                            , blockTimestamp = timestamp
                                            , blockPrevious  = previous
                                            }
 where
  hash = SHA256.hash $ BS.concat
    [Enc.encodeUtf8 content, Enc.encodeUtf8 . Text.pack $ timestamp, previous]

getTimestamp :: IO Timestamp
getTimestamp = formatTime defaultTimeLocale "%FT%T%9Q" <$> getCurrentTime
