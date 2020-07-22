{-# LANGUAGE OverloadedStrings #-}

module Spec
  ( main
  )
where

import           Test.Hspec

import qualified Blockchain                    as BC
import qualified Data.Text                     as Text
import qualified Data.ByteString               as ByteString

import           Data.Maybe                     ( isJust )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , parseTimeM
                                                )

main :: IO ()
main = hspec $ do

  describe "definition of a single block" $ do

    it "a block contains a hash" $ do
      let block = BC.newBlock Text.empty "" ByteString.empty
      BC.blockHash block `shouldNotBe` ByteString.empty

    it "a block stores some kind of data" $ do
      let content = "Hello World"
      let block   = BC.newBlock content "" ByteString.empty
      BC.blockContent block `shouldBe` content

    it "a block has a timestamp" $ do
      let timestamp = "2008-10-31T18:10:00.000000000"
      let block     = BC.newBlock Text.empty timestamp ByteString.empty
      BC.blockTimestamp block `shouldBe` timestamp

    it "a block stores the hash of the previous block" $ do
      let previous = ByteString.pack [1, 2, 3]
      let block    = BC.newBlock Text.empty "" previous
      BC.blockPrevious block `shouldBe` previous

  describe "properties of the hash of a block" $ do

    it "the hash varies with the stored data" $ do
      let b1 = BC.newBlock "hello" "" ByteString.empty
      let b2 = BC.newBlock "world" "" ByteString.empty
      BC.blockHash b1 `shouldNotBe` BC.blockHash b2

    it "the hash varies with different timestamp" $ do
      let
        b1 = BC.newBlock Text.empty
                         "2008-10-31T18:10:00.000000000"
                         ByteString.empty
      let
        b2 = BC.newBlock Text.empty
                         "2009-01-03T19:15:00.000000000"
                         ByteString.empty
      BC.blockHash b1 `shouldNotBe` BC.blockHash b2

    it "the hash varies with the hash of the previous block" $ do
      let b1 = BC.newBlock Text.empty "" (ByteString.pack [1, 2, 3, 4])
      let b2 = BC.newBlock Text.empty "" (ByteString.pack [5, 6, 7, 8])
      BC.blockHash b1 `shouldNotBe` BC.blockHash b2

  describe "creating a new blockchain" $ do

    it "new blockchain consists of a genesis block" $ do
      (BC.Blockchain [block]) <- BC.newBlockchain
      BC.blockContent block `shouldBe` "Genesis Block"

    it "a genesis block has no previous block" $ do
      (BC.Blockchain [block]) <- BC.newBlockchain
      BC.blockPrevious block `shouldBe` ByteString.empty

    it "a genesis block has a valid timestamp" $ do
      (BC.Blockchain [block]) <- BC.newBlockchain
      parseTimestamp block `shouldSatisfy` isJust

  describe "adding new blocks" $ do

    it "new blocks are appended" $ do
      (BC.Blockchain one) <- BC.newBlockchain >>= (`BC.addBlock` Text.empty)
      length one `shouldBe` 2
      (BC.Blockchain two) <- BC.addBlock (BC.Blockchain one) Text.empty
      length two `shouldBe` 3

    it "a new block contains some data" $ do
      let content = "block data"
      (BC.Blockchain blocks) <- BC.newBlockchain >>= (`BC.addBlock` content)
      (BC.blockContent . last) blocks `shouldBe` content

    it "any new block contains the hash of the previous block" $ do
      (BC.Blockchain [b1, b2, b3]) <-
        BC.newBlockchain
        >>= (`BC.addBlock` Text.empty)
        >>= (`BC.addBlock` Text.empty)
      BC.blockHash b1 `shouldBe` BC.blockPrevious b2
      BC.blockHash b2 `shouldBe` BC.blockPrevious b3

    it "new blocks are ordered chronologically" $ do
      (BC.Blockchain blocks) <-
        BC.newBlockchain
        >>= (`BC.addBlock` Text.empty)
        >>= (`BC.addBlock` Text.empty)
      let [t1, t2, t3] = map parseTimestamp blocks
      t1 `shouldSatisfy` (< t2)
      t2 `shouldSatisfy` (< t3)

  -- describe "proof of work" $ do

  --   it "all block hashes start with two 0 bytes" $ do
  --     (BC.Blockchain blocks) <- BC.newBlockchain >>= (`BC.addBlock` Text.empty)
  --     (ByteString.unpack . BC.blockHash . head) blocks `shouldStartWith` [0, 0]
  --     (ByteString.unpack . BC.blockHash . last) blocks `shouldStartWith` [0, 0]

parseTimestamp :: BC.Block -> Maybe UTCTime
parseTimestamp =
  parseTimeM True defaultTimeLocale "%FT%T%Q" . BC.blockTimestamp
