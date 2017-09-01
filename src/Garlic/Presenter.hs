module Garlic.Presenter where

import Control.Lens
import Garlic.Types
import Reactive.Banana
import GI.Gtk (Application)
import Database.Persist.Sql

import Garlic.View
import Garlic.View.HeaderBar

import Garlic.Model

presenter :: Application -> Garlic ()
presenter app' = do
    runMigration migrateAll
    app <- application app'

    initViews app

    app ^. appEnableSearch `consume` app ^. appHeader . searchToggled

    return ()

initViews :: GarlicApp -> Garlic ()
initViews app = do
    let x = app ^. appActivate
    return ()
