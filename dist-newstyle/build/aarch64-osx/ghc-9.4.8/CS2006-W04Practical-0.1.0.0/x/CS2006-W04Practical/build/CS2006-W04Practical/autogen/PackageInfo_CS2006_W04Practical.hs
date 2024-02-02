{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_CS2006_W04Practical (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "CS2006_W04Practical"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A simple text based interface game."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
