{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.HeaderBar where

import Garlic.Types
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Reactive.Banana.GI.Gtk (castB)
import GI.Gtk

uiHeaderBar :: Text
uiHeaderBar = decodeUtf8 $(embedFile "res/headerbar.ui")

headerBar :: Garlic HeaderBar
headerBar = do
    b <- builderNew
    _ <- builderAddFromString b uiHeaderBar (-1)

    castB b "headerBar" HeaderBar
