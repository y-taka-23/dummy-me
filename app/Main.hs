module Main where

import Web.DummyMe
import Web.DummyMe.Config

main :: IO ()
main = getConfig >>= runDummyMe
