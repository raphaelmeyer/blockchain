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

newtype Blockchain = Blockchain [Block] deriving (Show, Eq)

newBlockchain :: IO Blockchain
newBlockchain = do
  timestamp <- getTimestamp
  return (Blockchain [genesis timestamp])

addBlock :: Blockchain -> Text.Text -> IO Blockchain
addBlock (Blockchain blocks) blockData = do
  timestamp <- getTimestamp
  let block = newBlock blockData timestamp (blockHash (last blocks))
  return (Blockchain (blocks ++ [block]))

newtype Hash = Hash BS.ByteString deriving (Eq)

instance Show Hash where
  show (Hash bs) = concatMap (printf "%02x") $ BS.unpack bs

type Nonce = Int
type Timestamp = String

data Block = Block {
  blockContent :: Text.Text,
  blockHash :: Hash,
  blockPrevious :: Hash,
  blockNonce :: Nonce,
  blockTimestamp :: Timestamp
} deriving (Show, Eq)

genesis :: Timestamp -> Block
genesis timestamp =
  newBlock (Text.pack "Genesis Block") timestamp (Hash BS.empty)

newBlock :: Text.Text -> Timestamp -> Hash -> Block
newBlock content timestamp previous = Block { blockContent   = content
                                            , blockHash      = hash
                                            , blockPrevious  = previous
                                            , blockNonce     = nonce
                                            , blockTimestamp = timestamp
                                            }
 where
  (Hash prev) = previous
  given       = BS.concat
    [Enc.encodeUtf8 content, (Enc.encodeUtf8 . Text.pack) timestamp, prev]
  (hash, nonce) = findNonce given 0

findNonce :: BS.ByteString -> Nonce -> (Hash, Nonce)
findNonce given n | BS.take difficulty hash == zeroes = (Hash hash, n)
                  | otherwise                         = findNonce given (n + 1)
 where
  difficulty = 2
  zeroes     = BS.replicate difficulty 0
  nonce      = (Enc.encodeUtf8 . Text.pack . show) n
  hash       = SHA256.hash $ BS.concat [given, nonce]

getTimestamp :: IO Timestamp
getTimestamp = formatTime defaultTimeLocale "%FT%T%9Q" <$> getCurrentTime
