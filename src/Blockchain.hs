module Blockchain where

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Enc

import           Text.Printf                    ( printf )

newtype Blockchain = Blockchain [Block] deriving (Show, Eq)

newBlockchain :: Blockchain
newBlockchain = Blockchain [genesis]

addBlock :: Blockchain -> Text.Text -> Blockchain
addBlock (Blockchain blocks) blockData =
  Blockchain (blocks ++ [newBlock blockData (blockHash (last blocks))])

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
findNonce content (Hash previous) n = case BS.unpack (BS.take 2 hash) of
  [0, 0] -> (Hash hash, n)
  _      -> findNonce content (Hash previous) (n + 1)
 where
  nonce = Enc.encodeUtf8 (Text.pack (show n))
  hash  = SHA256.hash $ BS.concat [(Enc.encodeUtf8 content), previous, nonce]
