import           Test.Hspec

import qualified Blockchain                    as BC
import qualified Data.Text                     as Text
import qualified Data.ByteString               as ByteString

main :: IO ()
main = hspec $ do
  describe "blockchain" $ do
    it "new blockchain consists of a genesis block" $ do
      (BC.Blockchain blocks) <- BC.newBlockchain
      length blocks `shouldBe` 1

    it "genesis block has no predecessor" $ do
      (BC.Blockchain blocks) <- BC.newBlockchain
      BC.blockPrevious (head blocks) `shouldBe` BC.Hash ByteString.empty

    it "genesis block hash starts with 0000" $ do
      (BC.Blockchain blocks) <- BC.newBlockchain
      (show . BC.blockHash . head) blocks `shouldStartWith` "0000"

    it "new blocks are appended" $ do
      (BC.Blockchain one) <- BC.newBlockchain >>= (`BC.addBlock` Text.empty)
      length one `shouldBe` 2
      (BC.Blockchain two) <- BC.addBlock (BC.Blockchain one) Text.empty
      length two `shouldBe` 3

    it "block contains data" $ do
      let content = Text.pack "block data"
      (BC.Blockchain blocks) <- BC.newBlockchain >>= (`BC.addBlock` content)
      (BC.blockContent . last) blocks `shouldBe` content

    it "block hash starts with 0000" $ do
      (BC.Blockchain blocks) <- BC.newBlockchain >>= (`BC.addBlock` Text.empty)
      (show . BC.blockHash . last) blocks `shouldStartWith` "0000"

    it "blocks are linked to previous blocks" $ do
      (BC.Blockchain blocks) <-
        BC.newBlockchain
        >>= (`BC.addBlock` Text.empty)
        >>= (`BC.addBlock` Text.empty)
      BC.blockHash (head blocks) `shouldBe` BC.blockPrevious (blocks !! 1)
      BC.blockHash (blocks !! 1) `shouldBe` BC.blockPrevious (blocks !! 2)

  describe "block" $ do
    it "block contains data" $ do
      let content = Text.pack "Hello World"
      let block   = BC.newBlock content (BC.Hash ByteString.empty)
      BC.blockContent block `shouldBe` content

    it "block has a hash" $ do
      let block = BC.newBlock Text.empty (BC.Hash ByteString.empty)
      BC.blockHash block `shouldNotBe` BC.Hash ByteString.empty

    it "block has a link to previous block" $ do
      let previous = BC.Hash (ByteString.pack [1, 2, 3])
      let block    = BC.newBlock Text.empty previous
      BC.blockPrevious block `shouldBe` previous

    it "block hash varies with data" $ do
      let b1 = BC.newBlock (Text.pack "hello") (BC.Hash ByteString.empty)
      let b2 = BC.newBlock (Text.pack "world") (BC.Hash ByteString.empty)
      BC.blockHash b1 `shouldNotBe` BC.blockHash b2

    it "block hash varies with predecessor" $ do
      let b1 = BC.newBlock Text.empty (BC.Hash (ByteString.pack [1, 2, 3, 4]))
      let b2 = BC.newBlock Text.empty (BC.Hash (ByteString.pack [5, 6, 7, 8]))
      BC.blockHash b1 `shouldNotBe` BC.blockHash b2

  describe "timestamp" $ do
    it "TODO" pending
