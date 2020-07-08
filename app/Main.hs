module Main where

import qualified Blockchain                    as BC
import qualified Data.Text                     as Text

main :: IO ()
main =
  BC.newBlockchain
    >>= (`BC.addBlock` Text.pack "hello")
    >>= (`BC.addBlock` Text.pack "world")
    >>= (`BC.addBlock` Text.pack "this is ...")
    >>= (`BC.addBlock` Text.pack "... another block")
    >>= print
