module Blockchain where

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Enc

import           Text.Printf                    ( printf )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )

newtype Blockchain = Blockchain [Block]

data Block = Block {
  blockHash :: Hash,
  blockContent :: Text.Text,
  blockTimestamp :: Timestamp,
  blockPrevious :: Hash,
  blockNonce :: Nonce
}

type Hash = BS.ByteString
type Timestamp = String
type Nonce = Int

newBlockchain :: IO Blockchain
newBlockchain = do
  timestamp <- getTimestamp
  return (Blockchain [newBlock (Text.pack "Genesis Block") timestamp BS.empty])

addBlock :: Blockchain -> Text.Text -> IO Blockchain
addBlock (Blockchain blocks) content = do
  timestamp <- getTimestamp
  let block = newBlock content timestamp (blockHash . last $ blocks)
  return (Blockchain (blocks ++ [block]))

newBlock :: Text.Text -> Timestamp -> Hash -> Block
newBlock content timestamp previous = Block { blockHash      = hash
                                            , blockContent   = content
                                            , blockTimestamp = timestamp
                                            , blockPrevious  = previous
                                            , blockNonce     = nonce
                                            }
 where
  given = BS.concat
    [Enc.encodeUtf8 content, Enc.encodeUtf8 . Text.pack $ timestamp, previous]
  (nonce, hash) = findNonce given 0

findNonce :: BS.ByteString -> Nonce -> (Nonce, Hash)
findNonce given n | BS.all (== 0) start = (n, hash)
                  | otherwise           = findNonce given (n + 1)
 where
  difficulty = 2
  start      = BS.take difficulty hash
  nonce      = Enc.encodeUtf8 . Text.pack . show $ n
  hash       = SHA256.hash $ BS.concat [given, nonce]

getTimestamp :: IO Timestamp
getTimestamp = formatTime defaultTimeLocale "%FT%T%9Q" <$> getCurrentTime

instance Show Blockchain where
  show (Blockchain blocks) = concatMap ((++ "\n") . show) blocks

instance Show Block where
  show block = printf "%13.13v... %v ^%08.8v... %v"
                      (showHex . blockHash $ block)
                      (blockTimestamp block)
                      (showHex . blockPrevious $ block)
                      (blockContent block)
    where showHex = concatMap (printf "%02x") . BS.unpack :: Hash -> String
