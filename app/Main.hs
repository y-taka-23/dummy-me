{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 $ spock spockCfg $

        get root $
            text "Hello, Spock!"
