{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Garlic
( 
    garlic
)
where

import Control.Exception (catch, finally)
import Control.Monad (void)
import Control.Monad.Reader
import Data.Text (pack, unpack)
import Database.Persist.Sqlite hiding (Connection)
import Database.Sqlite
import GI.Gio (applicationRun)
import GI.Gtk
import Prelude hiding (init)
import Reactive.Banana.Frameworks
import System.FilePath.Posix
import System.Environment (lookupEnv, getEnv)
import System.Directory

import Garlic.Presenter

noLog :: LogFunc
noLog _ _ _ _ = return ()

runGtk :: SqlBackend -> IO ()
runGtk backend = void $ do
    void $ init Nothing
    Just app <- applicationNew Nothing []
    compile (runReaderT (presenter app) backend) >>= actuate
    applicationRun app Nothing

-- | Get DB location at @~/.local/share/garlic/garlic.db@. If the directory
-- does not exist, create it
dbLocation :: IO FilePath
dbLocation = do
    env <- lookupEnv "GARLIC_ENV"
    case env of
        Just "production" -> do
            home <- getEnv "HOME"
            let dir = home </> ".local" </> "share" </> "garlic"
            createDirectoryIfMissing True dir
            pure $ dir </> "garlic.db"

        _ -> pure "/tmp/garlic.db"

garlic :: IO ()
garlic = do
    connection <- open . pack =<< dbLocation
    backend <- wrapConnection connection noLog
    runGtk backend
        `catch` (\(e :: GError) -> gerrorMessage e >>= putStrLn . unpack)
        `finally` close' backend
