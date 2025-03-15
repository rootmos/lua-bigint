module Main where

import Test.Hspec

import qualified MySpec

main :: IO ()
main = hspec MySpec.spec
