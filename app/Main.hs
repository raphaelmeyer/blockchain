{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import qualified Blockchain                    as BC

main :: IO ()
main =
  BC.newBlockchain
    >>= (`BC.addBlock` "hello")
    >>= (`BC.addBlock` "world")
    >>= (`BC.addBlock` "this is ...")
    >>= (`BC.addBlock` "... another block")
    >>= print
