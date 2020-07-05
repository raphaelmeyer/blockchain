module Main where

import qualified Blockchain                    as BC
import qualified Data.Text                     as Text

main :: IO ()
main = print
  (BC.addBlock (BC.addBlock BC.newBlockchain (Text.pack "hello"))
               (Text.pack "world")
  )
