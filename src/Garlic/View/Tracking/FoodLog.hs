{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Garlic.View.Tracking.FoodLog 
(
    LogRecipe (..),
    GarlicTrackingFoodLog,
    foodLog
) 
where

import GI.Gtk
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Reactive.Banana.GI.Gtk
import Reactive.Banana
import Data.Text (Text, pack)
import Data.IORef
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed
import Text.Printf

import qualified Data.Map as M

import Garlic.Types
import Garlic.Data.Meal

uiLogHeader :: Text
uiLogHeader = decodeUtf8 $(embedFile "res/log-header.ui")

uiLogEntry :: Text
uiLogEntry = decodeUtf8 $(embedFile "res/log-entry.ui")

uiLogAdd :: Text
uiLogAdd = decodeUtf8 $(embedFile "res/log-add.ui")

data LogRecipe = LogRecipe
    { lrMeal :: Meal
    , lrName :: Text
    , lrKcal :: Double
    , lrProtein :: Double
    , lrCarbs :: Double
    , lrFat :: Double
    } deriving (Eq, Show, Ord, Read)

data GarlicTrackingFoodLog = GarlicTrackingFoodLog
    { _flInsert :: Consumer LogRecipe
    , _flAdding :: Event Meal
    }

foodLog :: Builder -> Garlic GarlicTrackingFoodLog
foodLog b = do
    list <- castB b "foodLogList" ListBox
    (e, r) <- buildMeals list
    pure $
        GarlicTrackingFoodLog
            (ioConsumer $ \LogRecipe {..} -> do
                 entry <- newEntry lrName lrKcal lrProtein lrCarbs lrFat
                 addEntry lrMeal entry list r)
            e

buildMeals :: ListBox -> Garlic (Event Meal, IORef (M.Map Meal Int))
buildMeals list = do
    b <- builderNew
    _ <- builderAddFromString b uiLogAdd (-1)

    popover <- castB b "popover" Popover

    btns <- mapM (addHeader list) . map (pack . show) $ allMeals

    popoverMeal <- liftIO $ newIORef Breakfast

    forM_ (zip allMeals btns) $ \(meal, btn) ->
        on btn #clicked $ do
            writeIORef popoverMeal meal
            popoverSetRelativeTo popover (Just btn)
            popoverPopup popover

    r <- liftIO . newIORef . M.fromList $ zipWith (,) allMeals [0..]
    pure (never,r)

addEntry ::
       MonadIO m
    => Meal
    -> ListBoxRow
    -> ListBox
    -> IORef (M.Map Meal Int)
    -> m ()
addEntry m r l ref =
    M.lookup m <$> liftIO (readIORef ref) >>= \case
        Nothing -> pure ()
        Just p -> do
            listBoxInsert l r (fromIntegral p)
            liftIO $ modifyIORef ref (M.adjust succ m)

addHeader :: MonadIO m => ListBox -> Text -> m Button
addHeader list t = do
    b <- builderNew
    _ <- builderAddFromString b uiLogHeader (-1)

    -- set header
    lbl <- castB b "headerTitle" Label
    set lbl [ #label := t ]

    -- append to list
    box <- castB b "box" ListBoxRow
    listBoxInsert list box (-1)

    castB b "headerAdd" Button

newEntry ::
       MonadIO m
    => Text         -- ^ Header
    -> Double       -- ^ Kcal
    -> Double       -- ^ Protein
    -> Double       -- ^ Carbs
    -> Double       -- ^ Fat
    -> m ListBoxRow
newEntry t kcal protein carbs fat = do
    b <- builderNew
    _ <- builderAddFromString b uiLogEntry (-1)

    -- set header
    name <- castB b "name" Label
    set name [ #label := t ]

    -- set nutrition
    kcalLbl <- castB b "kcal" Label
    set kcalLbl [ #label := fmtDouble False kcal ]
    proteinLbl <- castB b "protein" Label
    set proteinLbl [ #label := fmtDouble True protein ]
    carbsLbl <- castB b "carbs" Label
    set carbsLbl [ #label := fmtDouble True carbs ]
    fatLbl <- castB b "fat" Label
    set fatLbl [ #label := fmtDouble True fat ]
    
    -- append to list
    castB b "box" ListBoxRow

fmtDouble :: Bool -> Double -> Text
fmtDouble g x = pack $ printf "%.1f%s" x (if g then "g" else [])