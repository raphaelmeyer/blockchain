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

newtype Blockchain = Blockchain [Block] deriving (Eq)

data Block = Block {
  blockContent :: Text.Text,
  blockHash :: Hash,
  blockPrevious :: Hash,
  blockNonce :: Nonce,
  blockTimestamp :: Timestamp
} deriving (Eq)

type Hash = BS.ByteString
type Nonce = Int
type Timestamp = String

newBlockchain :: IO Blockchain
newBlockchain = do
  timestamp <- getTimestamp
  return (Blockchain [genesis timestamp])

addBlock :: Blockchain -> Text.Text -> IO Blockchain
addBlock (Blockchain blocks) blockData = do
  timestamp <- getTimestamp
  let block = newBlock blockData timestamp (blockHash (last blocks))
  return (Blockchain (blocks ++ [block]))

genesis :: Timestamp -> Block
genesis timestamp = newBlock (Text.pack "Genesis Block") timestamp BS.empty

newBlock :: Text.Text -> Timestamp -> Hash -> Block
newBlock content timestamp previous = Block { blockContent   = content
                                            , blockHash      = hash
                                            , blockPrevious  = previous
                                            , blockNonce     = nonce
                                            , blockTimestamp = timestamp
                                            }
 where
  given = BS.concat
    [Enc.encodeUtf8 content, (Enc.encodeUtf8 . Text.pack) timestamp, previous]
  (hash, nonce) = findNonce given 0

findNonce :: BS.ByteString -> Nonce -> (Hash, Nonce)
findNonce given n | BS.all (== 0) start = (hash, n)
                  | otherwise           = findNonce given (n + 1)
 where
  difficulty = 2
  start      = BS.take difficulty hash
  nonce      = (Enc.encodeUtf8 . Text.pack . show) n
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
