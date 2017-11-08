# confide

[Confide](https://github.com/estatico/confide) port to Haskell (using `GHC.Generics` as opposed to Shapeless to generate 
[FromConf](https://github.com/estatico/confide/blob/master/core/src/main/scala/io/estatico/confide/FromConf.scala) typeclass instances). Uses [deiko-config](https://hackage.haskell.org/package/deiko-config) to parse HOCON .conf file and read in types

## Usage

HOCON `.conf` file
```
a="hello"

b {
  y=true
  z {
    x=5
  }
}
```

```haskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test where

import Data.Config
import Data.Config.Generic

data Foo = Foo {x :: Integer} deriving (Generic, Show)
data Bar = Bar {y :: Bool, z :: Foo} deriving (Generic, Show)
data Baz = Baz {a :: T.Text, b :: Bar} deriving (Generic, Show)

instance FromConf Foo
instance FromConf Bar
instance FromConf Baz

main :: IO ()
main = do
  c <- loadConfig "project/path/to/confFile.conf"
  baz <- get @Baz "" c
  print baz

```

GHCI

```
$ main 
Baz {a = "hello", b = Bar {y = True, z = Foo {x = 5}}}
``
