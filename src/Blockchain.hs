module Blockchain where


import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Enc

import           Text.Printf                    ( printf )

newtype Hash = Hash BS.ByteString deriving (Eq)

instance Show Hash where
  show (Hash bs) = concatMap (printf "%02x") $ BS.unpack bs

data Block = Block {
  content :: Text.Text,
  digest :: Hash,
  previous :: Hash
}

-- newBlockchain :: Blockchain
-- addBlock :: Blockchain -> Text.Text -> Blockchain

genesis :: Block
genesis = newBlock (Text.pack "Genesis Block") (Hash BS.empty)

newBlock :: Text.Text -> Hash -> Block
newBlock c p = Block { content = c, digest = findHash c p 0, previous = p }

type Nonce = Int

findHash :: Text.Text -> Hash -> Nonce -> Hash
findHash c (Hash p) n = case BS.unpack (BS.take 3 h) of
  [0, 0, 0] -> Hash h
  _         -> findHash c (Hash p) (n + 1)
 where
  nonce = Enc.encodeUtf8 (Text.pack (show n))
  h     = SHA256.hash $ foldr BS.append (Enc.encodeUtf8 c) [p, nonce]
