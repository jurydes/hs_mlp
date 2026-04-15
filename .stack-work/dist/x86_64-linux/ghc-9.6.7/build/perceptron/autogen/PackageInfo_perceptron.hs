{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_perceptron (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "perceptron"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "\1056\1077\1072\1083\1080\1079\1072\1094\1080\1103 \1072\1083\1075\1086\1088\1080\1090\1084\1072 \1087\1077\1088\1094\1077\1087\1090\1088\1086\1085\1072 \8212 \1082\1091\1088\1089\1086\1074\1086\1081 \1087\1088\1086\1077\1082\1090 \1087\1086 ML"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
