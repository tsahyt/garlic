{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Garlic.View.Tracking.FoodLog 
(
    LogRecipe (..),
    GarlicTrackingFoodLog,
    flInsert,
    flClean,
    flAdding,
    flDelete,
    flName,
    flAmount,
    flLoadRecipes,
    flAmountEdit,
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
import qualified Data.ListMap as L

import Garlic.Types
import Garlic.Data.Meal
import Garlic.Model (FoodEntryId)

uiLogHeader :: Text
uiLogHeader = decodeUtf8 $(embedFile "res/log-header.ui")

uiLogEntry :: Text
uiLogEntry = decodeUtf8 $(embedFile "res/log-entry.ui")

uiLogAdd :: Text
uiLogAdd = decodeUtf8 $(embedFile "res/log-add.ui")

data LogRecipe = LogRecipe
    { lrMeal :: Meal
    , lrName :: Text
    , lrAmount :: Double
    , lrKcal :: Double
    , lrProtein :: Double
    , lrCarbs :: Double
    , lrFat :: Double
    , lrKey :: FoodEntryId
    } deriving (Eq, Show, Ord, Read)

data GarlicTrackingFoodLog = GarlicTrackingFoodLog
    { _flInsert      :: Consumer LogRecipe
    , _flClean       :: Consumer ()
    , _flAdding      :: Event Meal
    , _flDelete      :: Event LogRecipe
    , _flName        :: Behavior Text
    , _flAmount      :: Behavior Double
    , _flLoadRecipes :: Consumer [Text]
    , _flAmountEdit  :: Behavior Double -- (Double, FoodEntryId)
    }

foodLog :: Builder -> Garlic GarlicTrackingFoodLog
foodLog b = do
    mref <- liftIO $ newIORef []
    list <- castB b "foodLogList" ListBox
    (e, pref, name, amount, load) <- buildMeals list
    del <- deletion b list pref mref
    amountEdit <- castB b "foodLogEditAmountAdjustment" Adjustment
    amountEditing list amountEdit mref

    GarlicTrackingFoodLog <$>
        pure
            (ioConsumer $ \lr@LogRecipe {..} -> do
                 entry <- newEntry lrName lrKcal lrProtein lrCarbs lrFat
                 addEntry lr lrMeal entry list pref mref) <*>
        pure (ioConsumer $ \_ -> cleanEntries list pref mref) <*>
        pure e <*>
        pure del <*>
        pure name <*>
        pure amount <*>
        pure load <*>
        lift (attrB amountEdit #value)

amountEditing ::
       MonadIO m
    => ListBox
    -> Adjustment
    -> IORef [Either Meal LogRecipe]
    -> m ()
amountEditing list adjustment ref =
    void $
    on list #rowSelected $ \case
        Nothing -> pure ()
        Just row -> do
            m <- readIORef ref
            idx <- fromIntegral <$> listBoxRowGetIndex row
            case L.atMay idx m of
                Just (Right lr) -> adjustmentSetValue adjustment (lrAmount lr)
                _ -> pure ()

deletion ::
       Builder
    -> ListBox
    -> IORef (M.Map Meal Int)
    -> IORef [Either Meal LogRecipe]
    -> Garlic (Event LogRecipe)
deletion b list pref mref = do
    btn <- castB b "foodLogDelete" Button
    lift $
        signalEN btn #clicked $ \h -> do
            m <- readIORef mref
            row <- listBoxGetSelectedRow list
            idx <- fromIntegral <$> listBoxRowGetIndex row
            case L.atMay idx m of
                Just (Right lr) -> do
                    containerRemove list row
                    modifyIORef pref (M.adjust pred (lrMeal lr))
                    modifyIORef mref (L.delete idx)
                    h lr
                _ -> pure ()

buildMeals ::
       ListBox
    -> Garlic ( Event Meal
              , IORef (M.Map Meal Int)
              , Behavior Text
              , Behavior Double
              , Consumer [Text] )
buildMeals list = do
    b <- builderNew
    _ <- builderAddFromString b uiLogAdd (-1)

    popover <- castB b "popover" Popover
    name <- castB b "name" Entry
    nameB <- lift $ attrB name #text
    servings <- castB b "servingAdjustment" Adjustment
    servingsB <- lift $ attrB servings #value
    okButton <- castB b "okButton" Button

    _ <- on okButton #clicked $ popoverPopdown popover

    btns <- mapM (addHeader list . pack . show) allMeals

    popoverMeal <- liftIO $ newIORef Breakfast

    forM_ (zip allMeals btns) $ \(meal, btn) ->
        on btn #clicked $ do
            writeIORef popoverMeal meal
            entrySetText name ""
            adjustmentSetValue servings 1.0
            popoverSetRelativeTo popover (Just btn)
            popoverPopup popover

    r <- liftIO . newIORef . M.fromList . map (\x -> (x,0)) $ allMeals

    adding <- lift $ signalEN okButton #clicked $ \h ->
                readIORef popoverMeal >>= h

    compl <- castB b "recipeCompletion" EntryCompletion
    loadCompl <- completion compl

    pure (adding,r,nameB,servingsB,loadCompl)

completion :: EntryCompletion -> Garlic (Consumer [Text])
completion comp = do
    model <- listStoreNew [ gtypeString ]
    entryCompletionSetModel comp (Just model)

    trender <- new CellRendererText []
    Just area <- get comp #cellArea
    cellLayoutPackStart area trender True
    cellLayoutAddAttribute area trender "text" 0

    let load xs = do
            listStoreClear model
            forM_ xs $ \(x :: Text) -> do
                iter <- listStoreAppend model
                x' <- toGValue (Just x)
                listStoreSetValue model iter 0 x'

    pure $ ioConsumer load

cleanEntries ::
       MonadIO m
    => ListBox
    -> IORef (M.Map Meal Int)
    -> IORef [Either Meal LogRecipe]
    -> m ()
cleanEntries list pref mref = do
    m <- liftIO $ readIORef pref
    m' <- go m
    liftIO $ do
        writeIORef pref m'
        writeIORef mref (map Left allMeals)
  where
    go m = do
        let open = M.filter (> 0) m
        if M.null open
            then pure m
            else do
                let meal = head $ M.keys open
                    x = fromIntegral . succ . fromEnum $ meal
                r <- listBoxGetRowAtIndex list x
                case r of
                    Nothing -> pure m
                    Just r' ->
                        containerRemove list r' >> go (M.adjust pred meal m)

addEntry ::
       MonadIO m
    => LogRecipe
    -> Meal
    -> ListBoxRow
    -> ListBox
    -> IORef (M.Map Meal Int)
    -> IORef [Either Meal LogRecipe]
    -> m ()
addEntry lr m row l pref mref = do
    before <- takeWhile ((/= m) . fst) . M.toList <$> liftIO (readIORef pref)
    let p = fromIntegral (sum . map (\(_,x) -> succ x) $ before) + 1
    listBoxInsert l row p
    liftIO $ do
        modifyIORef pref (M.adjust succ m)
        modifyIORef mref (L.insert (fromIntegral p) (Right lr))

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

-- LENSES
makeGetters ''GarlicTrackingFoodLog
