module Blockchain where

-- import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
-- import qualified Data.Text.Encoding            as Enc

-- import           Text.Printf                    ( printf )
-- import           Data.Time.Clock                ( getCurrentTime )
-- import           Data.Time.Format               ( defaultTimeLocale
--                                                 , formatTime
--                                                 )

data Block = Block {
  blockHash :: Hash,
  blockContent :: Text.Text,
  blockTimestamp :: Timestamp,
  blockPrevious :: Hash
}

type Hash = BS.ByteString
type Timestamp = String

newBlock :: Text.Text -> Timestamp -> Hash -> Block
newBlock content timestamp previous = Block { blockHash      = BS.pack [1, 2, 3]
                                            , blockContent   = content
                                            , blockTimestamp = timestamp
                                            , blockPrevious  = previous
                                            }
