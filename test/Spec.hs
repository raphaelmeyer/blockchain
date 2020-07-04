import           Test.Hspec

import           Blockchain
import           Data.Text                     as Text
import           Data.ByteString               as ByteString

main :: IO ()
main = hspec $ do
  describe "genesis block" $ do
    it "has some content" $ do
      content genesis `shouldBe` Text.pack "Genesis Block"

    it "has a digest" $ do
      digest genesis `shouldNotBe` Hash ByteString.empty

    it "has no predecessor" $ do
      previous genesis `shouldBe` Hash ByteString.empty

    it "digest must start with 000" $ do
      show (digest genesis) `shouldStartWith` "000"

  describe "block" $ do
    it "contains data" $ do
      let blockData = Text.pack "Hello World"
      let block     = newBlock blockData (Hash ByteString.empty)
      content block `shouldBe` blockData

    it "has a digest" $ do
      let block = newBlock (Text.pack "Some data") (Hash ByteString.empty)
      digest block `shouldNotBe` Hash ByteString.empty

    it "has a link to the previous block" $ do
      let b1 = newBlock (Text.pack "data") (digest genesis)
      let b2 = newBlock (Text.pack "data") (digest b1)
      previous b1 `shouldBe` digest genesis
      previous b2 `shouldBe` digest b1

    it "digest varies with data" $ do
      let b1 = newBlock (Text.pack "hello") (digest genesis)
      let b2 = newBlock (Text.pack "world") (digest genesis)
      digest b1 `shouldNotBe` digest b2

    it "digest varies with predecessor" $ do
      let b1 = newBlock (Text.pack "same") (digest genesis)
      let b2 = newBlock (Text.pack "same") (digest b1)
      digest b1 `shouldNotBe` digest b2

    it "digest must start with 000" $ do
      let block = newBlock (Text.pack "some block") (digest genesis)
      show (digest block) `shouldStartWith` "000"
