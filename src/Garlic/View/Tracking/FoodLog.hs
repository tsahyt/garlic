{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Garlic.View.Tracking.FoodLog 
(
    Adding (..),
    LogRecipe (..),
    GarlicTrackingFoodLog,
    flInsert,
    flClean,
    flAdding,
    flDelete,
    flLoadRecipes,
    flAmountEdit,
    foodLog
) 
where

import GI.Gtk hiding (Unit(..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Reactive.Banana.GI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks (newEvent, mapEventIO)
import Data.Text (Text, pack)
import Data.IORef
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed
import Text.Printf

import qualified Data.Map as M
import qualified Data.ListMap as L

import Garlic.Types
import Garlic.Data.Meal
import Garlic.Data.Units
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

type LogUpdate = Double -> Double -> Double -> Double -> IO ()

data Adding
    = AddingRecipe Meal Text Double
    | AddingIngredient Meal Text Double Unit
    deriving (Eq, Show, Ord, Read)

setMeal :: Adding -> Meal -> Adding
setMeal (AddingRecipe _ t d) m = AddingRecipe m t d
setMeal (AddingIngredient _ t d u) m = AddingIngredient m t d u

data GarlicTrackingFoodLog = GarlicTrackingFoodLog
    { _flInsert      :: Consumer LogRecipe
    , _flClean       :: Consumer ()
    , _flAdding      :: Event Adding
    , _flDelete      :: Event LogRecipe
    , _flLoadRecipes :: Consumer [Text]
    , _flAmountEdit  :: Event (Double, FoodEntryId)
    }

foodLog :: Builder -> Garlic EntryCompletion -> Garlic GarlicTrackingFoodLog
foodLog b newCompl = do
    mref <- liftIO $ newIORef []
    list <- castB b "foodLogList" ListBox
    (e, pref, addingB, load) <- buildMeals list newCompl
    del <- deletion b list pref mref
    edit <- amountEditing b list mref
    GarlicTrackingFoodLog <$>
        pure
            (ioConsumer $ \lr@LogRecipe {..} -> do
                 (entry, upd) <- newEntry lrName lrKcal lrProtein lrCarbs lrFat
                 addEntry lr upd lrMeal entry list pref mref) <*>
        pure (ioConsumer $ \_ -> cleanEntries list pref mref) <*>
        pure ((setMeal <$> addingB) <@> e) <*>
        pure del <*>
        pure load <*>
        pure edit

amountEditing ::
       Builder
    -> ListBox
    -> IORef [Either Meal (LogRecipe, LogUpdate)]
    -> Garlic (Event (Double, FoodEntryId))
amountEditing b list ref = do
    (e, handle) <- lift newEvent
    amountEdit <- castB b "foodLogEditAmountAdjustment" Adjustment
    -- load previous value on selection
    void $
        on list #rowSelected $ \case
            Nothing -> pure ()
            Just row -> do
                m <- readIORef ref
                idx <- fromIntegral <$> listBoxRowGetIndex row
                case L.atMay idx m of
                    Just (Right (lr, _)) ->
                        adjustmentSetValue amountEdit (lrAmount lr)
                    _ -> pure ()
    -- trigger event on change
    void $
        on amountEdit #valueChanged $ do
            m <- readIORef ref
            val <- adjustmentGetValue amountEdit
            row <- listBoxGetSelectedRow list
            idx <- fromIntegral <$> listBoxRowGetIndex row
            case L.atMay idx m of
                Just (Right (lr, update)) -> do
                    let factor = val / lrAmount lr
                        kcal = factor * lrKcal lr
                        protein = factor * lrProtein lr
                        carbs = factor * lrCarbs lr
                        fat = factor * lrFat lr
                    update kcal protein carbs fat
                    handle (val, lrKey lr)
                _ -> pure ()
    pure e

deletion ::
       Builder
    -> ListBox
    -> IORef (M.Map Meal Int)
    -> IORef [Either Meal (LogRecipe, a)]
    -> Garlic (Event LogRecipe)
deletion b list pref mref = do
    btn <- castB b "foodLogDelete" Button
    lift $
        signalEN btn #clicked $ \h -> do
            m <- readIORef mref
            row <- listBoxGetSelectedRow list
            idx <- fromIntegral <$> listBoxRowGetIndex row
            case L.atMay idx m of
                Just (Right (lr,_)) -> do
                    containerRemove list row
                    modifyIORef pref (M.adjust pred (lrMeal lr))
                    modifyIORef mref (L.delete idx)
                    h lr
                _ -> pure ()

data PopoverActive
    = RecipeActive
    | IngredientActive
    deriving (Eq, Show, Ord, Read)

buildMeals ::
       ListBox
    -> Garlic EntryCompletion
    -> Garlic ( Event Meal
              , IORef (M.Map Meal Int)
              , Behavior Adding
              , Consumer [Text])
buildMeals list newCompl = do
    b <- builderNew
    _ <- builderAddFromString b uiLogAdd (-1)
    popover <- castB b "popover" Popover
    switcher <- castB b "switcher" StackSwitcher
    stack <- castB b "stack" Stack

    -- behavior denoting the currently active popover page
    activeB <-
        do (e, h) <- lift newEvent
           _ <-
               on switcher #buttonReleaseEvent $ \_ -> do
                   name <- stackGetVisibleChildName stack
                   case name of
                       Just "recipes" -> h RecipeActive
                       Just "ingredients" -> h IngredientActive
                       _ -> pure ()
                   return True
           stepper RecipeActive e

    -- popover widgets
    rname <- castB b "recipeName" Entry
    iname <- castB b "ingredientName" Entry
    rservings <- castB b "recipeServing" Adjustment
    iamount <- castB b "ingredientAmount" Adjustment

    -- instantiate completion for iname
    icompl <- newCompl
    set iname [ #completion := icompl ]
 
    -- fill unit box
    unit <- castB b "units" ComboBoxText
    mapM_ (comboBoxTextAppendText unit . prettyUnit) allUnits

    -- adding behavior
    addingB <-
        do rnameB <- lift $ attrB rname #text
           rservingsB <- lift $ attrB rservings #value
           inameB <- lift $ attrB iname #text
           iamountB <- lift $ attrB iamount #value

           c  <- lift $ signalE0 unit #changed
           c' <- lift $ mapEventIO (const $ comboBoxTextGetActiveText unit) c
           unitB <- stepper Gram $ parseUnit <$> c'

           pure $
               dataToAdding <$> activeB <*> rnameB <*> rservingsB <*> inameB <*>
               iamountB <*> unitB

    okButton <- castB b "okButton" Button
    _ <- on okButton #clicked $ popoverPopdown popover
    btns <- mapM (addHeader list . pack . show) allMeals
    popoverMeal <- liftIO $ newIORef Breakfast

    -- popover handling per meal button
    forM_ (zip allMeals btns) $ \(meal, btn) ->
        on btn #clicked $ do
            writeIORef popoverMeal meal
            entrySetText rname ""
            entrySetText iname ""
            adjustmentSetValue rservings 1.0
            adjustmentSetValue iamount 0.0
            popoverSetRelativeTo popover (Just btn)
            popoverPopup popover
    r <- liftIO . newIORef . M.fromList . map (\x -> (x, 0)) $ allMeals
    adding <-
        lift $ signalEN okButton #clicked $ \h -> readIORef popoverMeal >>= h

    -- recipe completion handling
    compl <- castB b "recipeCompletion" EntryCompletion
    loadCompl <- completion compl
    pure (adding, r, addingB, loadCompl)

dataToAdding ::
       PopoverActive
    -> Text -- ^ recipe name
    -> Double -- ^ recipe amount
    -> Text -- ^ ingredient name
    -> Double -- ^ ingredient amount
    -> Unit -- ^ ingredient unit
    -> Adding
dataToAdding RecipeActive r a _ _ _ = AddingRecipe Breakfast r a
dataToAdding IngredientActive _ _ i a u = AddingIngredient Breakfast i a u

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
    -> IORef [Either Meal (LogRecipe, a)]
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
    -> LogUpdate
    -> Meal
    -> ListBoxRow
    -> ListBox
    -> IORef (M.Map Meal Int)
    -> IORef [Either Meal (LogRecipe, LogUpdate)]
    -> m ()
addEntry lr lu m row l pref mref = do
    before <- takeWhile ((/= m) . fst) . M.toList <$> liftIO (readIORef pref)
    let p = fromIntegral (sum . map (\(_,x) -> succ x) $ before) + 1
    listBoxInsert l row p
    liftIO $ do
        modifyIORef pref (M.adjust succ m)
        modifyIORef mref (L.insert (fromIntegral p) (Right (lr, lu)))

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
    -> m (ListBoxRow, LogUpdate)
newEntry t kcal protein carbs fat = do
    b <- builderNew
    _ <- builderAddFromString b uiLogEntry (-1)

    -- set header
    name <- castB b "name" Label
    set name [ #label := t ]

    -- get widgets
    kcalLbl <- castB b "kcal" Label
    proteinLbl <- castB b "protein" Label
    carbsLbl <- castB b "carbs" Label
    fatLbl <- castB b "fat" Label
    
    -- create update function holding widgets in closure
    let update kcal' protein' carbs' fat' = do
            set kcalLbl [ #label := fmtDouble False kcal' ]
            set proteinLbl [ #label := fmtDouble True protein' ]
            set carbsLbl [ #label := fmtDouble True carbs' ]
            set fatLbl [ #label := fmtDouble True fat' ]

    -- set nutrition
    update kcal protein carbs fat
    
    -- append to list
    row <- castB b "box" ListBoxRow

    pure (row, update)

fmtDouble :: Bool -> Double -> Text
fmtDouble g x = pack $ printf "%.1f%s" x (if g then "g" else [])

-- LENSES
makeGetters ''GarlicTrackingFoodLog
