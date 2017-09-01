{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter where

import Control.Lens
import Database.Persist.Sql
import GI.Gtk (Application)
import Garlic.Types

import Garlic.Model
import Garlic.View
import Garlic.View.HeaderBar

presenter :: Application -> Garlic ()
presenter app' = do
    runMigration migrateAll
    app <- application app'

    -- Toggle Search Bar
    app ^. appEnableSearch `consume` app ^. appHeader . searchToggled

    return ()
