{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Garlic.Presenter.RecipeEdit
(
    recipeEditP
)
where

import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.Functor.Compose
import Data.Sequence (Seq)
import Database.Persist.Sql
import Reactive.Banana

import Garlic.Data.Duration
import Garlic.Model
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.RecipeDisplay
import Garlic.View.RecipeEdit
import Garlic.View.IngredientEditor
import Garlic.Presenter.IngredientEditor

import qualified Data.Sequence as S

recipeEditP
    :: GarlicApp
    -> Event (Entity Recipe)
    -> Garlic (Event (Seq (Entity Recipe) -> Seq (Entity Recipe)))
recipeEditP app selected = do
    key <- stepper Nothing (Just . entityKey <$> selected)

    -- Show Editor on Edit or Add Click, hide Edit Button and yield
    let click = app ^. appHeader . editClick
            <:> app ^. appHeader . addClick
     in do app ^. appRecipeEdit . showEditor `consume` click
           app ^. appHeader . yieldToggle `consume` click
           app ^. appHeader . editToggle `consume` click
           app ^. appHeader . addToggle `consume` click
           app ^. appHeader . backToggle `consume` click

    recipe <- currentRecipe app
    
    -- Ingredients
    new <- newIngredientPopover app
    selectedIngredients <- loadRecipe app selected
    completion <- fetch allIngredientNames $ () <$ new
    app ^. appReplaceIngr `consume` completion

    entered <- fetch ingredientByName 
        $ app ^. appRecipeEdit . editEnterIngredient

    ingredients <- ingredientList 
        (app ^. appRecipeEdit . editIngredients) 
        selectedIngredients
        (new <:> entered)

    -- Recipe Entity, only Just when there is also a previous selection
    let recipeEntity = getCompose $ 
            (,) <$> (Entity <$> Compose key 
                            <*> Compose (Just <$> recipe))
                <*> Compose (Just <$> ingredients)
    
    -- Save on Store Click, or do nothing when there was no selection
    let storeE :: Event (Entity Recipe, [WeighedIngredient])
        storeE = filterJust (recipeEntity <@ app ^. appRecipeEdit . editStore)
    updateRecipe `consume` storeE

    -- Show Display on Abort Click
    revertDisplay app $ app ^. appHeader . backClick

    -- Delete Selected Recipe on Delete, revert to display view
    let deleteE = filterJust (key <@ app ^. appRecipeEdit . editDelete)
    deleteRecipe `consume` deleteE
    revertDisplay app deleteE

    pure $ unions
        [ (\x -> S.filter ((/= x) . entityKey)) <$> deleteE 
        , (\x -> fmap (\e -> if entityKey e == entityKey x then x else e)) 
        . fst <$> storeE ]

revertDisplay :: GarlicApp -> Event a -> Garlic ()
revertDisplay app e = do
    app ^. appRecipeDisplay . showDisplay `consume` () <$ e
    app ^. appHeader . yieldToggle `consume` () <$ e
    app ^. appHeader . editToggle `consume` () <$ e
    app ^. appHeader . addToggle  `consume` () <$ e
    app ^. appHeader . backToggle  `consume` () <$ e

-- | Load recipe into mask on selection event
loadRecipe 
    :: GarlicApp 
    -> Event (Entity Recipe) 
    -> Garlic (Event [WeighedIngredient])
loadRecipe app selected = do
    let masks = app ^. appRecipeEdit . editMasks
        rcp   = entityVal <$> selected
    masks ^. editSetName `consume` recipeName <$> rcp
    masks ^. editSetCuisine `consume` recipeCuisine <$> rcp
    masks ^. editSetDuration `consume` durationString . recipeDuration <$> rcp
    masks ^. editSetYield `consume` recipeYield <$> rcp
    masks ^. editSetYieldUnit `consume` recipeYieldUnit <$> rcp
    masks ^. editSetSource `consume` fromMaybe "" . recipeSource <$> rcp
    masks ^. editSetURL `consume` fromMaybe "" . recipeUrl <$> rcp
    masks ^. editSetRating `consume` recipeRating <$> rcp

    app ^. appRecipeEdit . editSetInstructions 
        `consume` recipeInstructions <$> rcp

    fetch ingredientsFor (entityKey <$> selected)

-- | The currently edited recipe
currentRecipe :: GarlicApp -> Garlic (Behavior Recipe)
currentRecipe app = do
    let masks = app ^. appRecipeEdit . editMasks
        r     = Recipe 
            <$> masks ^. editName
            <*> masks ^. editCuisine
            <*> masks ^. editRating
            <*> fmap (fromMaybe "") (app ^. appRecipeEdit . editInstructions)
            <*> fmap (fromMaybe 0 . parseDuration) (masks ^. editDuration)
            <*> masks ^. editYield
            <*> masks ^. editYieldUnit
            <*> fmap mtext (masks ^. editSource)
            <*> fmap mtext (masks ^. editURL)
    pure r

-- | Network describing the new ingredient dialog
newIngredientPopover :: GarlicApp -> Garlic (Event (Entity Ingredient))
newIngredientPopover app = do
    let ni = app ^. appRecipeEdit . editNewIngredient

    ni ^. niMask ^. imClearAll `consume` ni ^. niClearClick
    ni ^. niMask ^. imClearAll `consume` ni ^. niOkClick

    new <- fetch newIngredient $ 
        currentIngredient (ni ^. niMask) <@ ni ^. niOkClick
    app ^. appDisplayError `consume`
        "Ingredient already exists!" <$ filterE isNothing new
    
    pure (filterJust new)

-- | Management of the ingredient list
ingredientList 
    :: GarlicIngredientList 
    -> Event [WeighedIngredient] 
    -> Event (Entity Ingredient)
    -> Garlic (Behavior [WeighedIngredient])
ingredientList ilist selected new = mdo
    let fromWI w = EditorIngredient
                       (view wingrAmount w)
                       (view wingrUnit w)
                       (view (wingrIngr . to entityVal . to ingredientName) w)
                       (view wingrDisp w)
                       (view wingrOptional w)
        
        toWI i = updWI i . fromIngredient

        updWI EditorIngredient{..} = 
            set wingrOptional eiOptional . set wingrDisp eiDisplay 
          . set wingrUnit eiUnit . set wingrAmount eiAmount

    -- Replace ingredient list on new selection
    ((() >$ ilist ^. ilClear) <> ilist ^. ilAppend) `consume` 
        map fromWI <$> selected

    -- Insert new ingredients
    ilist ^. ilAppend `consume` (pure . fromWI . fromIngredient) <$> new

    -- Events that can affect the managed list of ingredients
    let clearE   = const [] <$ selected
        appendE  = (flip (++) . pure) . fromIngredient <$> new
        deleteE  = delete' <$> ilist ^. ilDeleted
        changedE = (\(i,x) xs -> modify' i xs (updWI x))
               <$> whenE (not . null <$> is) (ilist ^. ilChanged)

    insertE <- do
        r  <- spread $ zip [0..] <$> selected
        let x = ilist ^. ilInserted
            f (i,a) b = (i, toWI a b)
        xB <- stepper (error "insertE: empty stepper") $ x
        e  <- fetch ingredientByName $ view (_2 . to eiName) <$> x
        pure $ uncurry (flip . insert') <$> ((f <$> xB <@> e) <:> r)

    is <- accumB [] $ unions [ deleteE, clearE, changedE, insertE, appendE ]
    pure is

-- | Modify a list element at the specified index
modify' :: Int -> [a] -> (a -> a) -> [a]
modify' _ [] _ = []
modify' n xs f =
    case splitAt n xs of
        (l,[])  -> l
        (l,x:r) -> l ++ [f x] ++ r

-- | Insert element into list by index
insert' :: Int -> [a] -> a -> [a]
insert' 0 xs y = y : xs
insert' n xs y =
    case xs of
        []      -> pure y
        (x:xs') -> x : insert' (pred n) xs' y

-- | Delete element from list by index
delete' :: Int -> [a] -> [a]
delete' 0 xs = drop 1 xs
delete' n xs =
    case xs of
        [] -> []
        (x:xs') -> x : delete' (pred n) xs'
