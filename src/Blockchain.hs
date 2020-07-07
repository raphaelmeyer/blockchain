module Blockchain where

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Enc

import           Text.Printf                    ( printf )

newtype Blockchain = Blockchain [Block] deriving (Show, Eq)

newBlockchain :: IO Blockchain
newBlockchain = return (Blockchain [genesis])

addBlock :: Blockchain -> Text.Text -> IO Blockchain
addBlock (Blockchain blocks) blockData = do
  let block = newBlock blockData (blockHash (last blocks))
  return (Blockchain (blocks ++ [block]))

newtype Hash = Hash BS.ByteString deriving (Eq)

instance Show Hash where
  show (Hash bs) = concatMap (printf "%02x") $ BS.unpack bs

type Nonce = Int

data Block = Block {
  blockContent :: Text.Text,
  blockHash :: Hash,
  blockPrevious :: Hash,
  blockNonce :: Nonce
} deriving (Show, Eq)

genesis :: Block
genesis = newBlock (Text.pack "Genesis Block") (Hash BS.empty)

newBlock :: Text.Text -> Hash -> Block
newBlock content previous = Block { blockContent  = content
                                  , blockHash     = hash
                                  , blockPrevious = previous
                                  , blockNonce    = nonce
                                  }
  where (hash, nonce) = findNonce content previous 0

findNonce :: Text.Text -> Hash -> Nonce -> (Hash, Nonce)
findNonce content (Hash previous) n
  | BS.take difficulty hash == zeroes = (Hash hash, n)
  | otherwise = findNonce content (Hash previous) (n + 1)
 where
  difficulty = 2
  zeroes     = BS.replicate difficulty 0
  nonce      = Enc.encodeUtf8 (Text.pack (show n))
  hash       = SHA256.hash $ BS.concat [Enc.encodeUtf8 content, previous, nonce]
