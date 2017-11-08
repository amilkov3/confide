
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Data.Config.Generic
import Data.Config
import System.IO.Unsafe
import GHC.Generics
import Data.Text as T

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

data Foo = Foo {x :: Integer} deriving (Generic, Show, Eq)
data Bar = Bar {y :: Bool, z :: Foo} deriving (Generic, Show, Eq)
data Baz = Baz {a :: T.Text, b :: Bar} deriving (Generic, Show, Eq)

instance FromConf Foo
instance FromConf Bar
instance FromConf Baz

myBaz = Baz "hello" (Bar True (Foo 5))

unitTests = testGroup "Unit tests"
  [ testCase "Load in type from conf" $ unsafePerformIO (do
      c <- loadConfig "test/test.conf"
      baz <- get @Baz "" c
      return $ assertEqual "test" baz myBaz)
  ]
