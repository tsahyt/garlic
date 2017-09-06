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
import Data.Text (pack)
import Data.Sequence (Seq)
import Database.Persist.Sql
import Reactive.Banana

import Garlic.Model
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.RecipeEdit
import Garlic.View.RecipeDisplay

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

    recipe <- currentRecipe app
    
    -- Ingredients
    new <- ingredientEditor app
    selectedIngredients <- loadRecipe app selected
    completion <- fetch completionList (app ^. appStartup <:> (() <$ new))
    app ^. appRecipeEdit . editReplaceIngCompl `consume` completion
    entered <- fetch ingredientByName 
        $ app ^. appRecipeEdit . editEnterIngredient

    ingredients <- ingredientList 
        (app ^. appRecipeEdit . editIngredients) 
        selectedIngredients
        (new <:> entered)

    consume stdout . fmap show =<< plainChanges ingredients

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
    revertDisplay app $ app ^. appRecipeEdit . editAbort

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
    masks ^. editSetDuration `consume` pack . show . recipeDuration <$> rcp
    masks ^. editSetYield `consume` recipeYield <$> rcp
    masks ^. editSetYieldUnit `consume` recipeYieldUnit <$> rcp
    masks ^. editSetSource `consume` fromMaybe "" . recipeSource <$> rcp
    masks ^. editSetURL `consume` fromMaybe "" . recipeUrl <$> rcp
    masks ^. editSetRating `consume` recipeRating <$> rcp

    app ^. appRecipeEdit . editSetInstructions 
        `consume` recipeInstructions <$> rcp

    fetch ingredientsFor (entityKey <$> selected)

currentRecipe :: GarlicApp -> Garlic (Behavior Recipe)
currentRecipe app = do
    let masks = app ^. appRecipeEdit . editMasks
        r     = Recipe 
            <$> masks ^. editName
            <*> masks ^. editCuisine
            <*> masks ^. editRating
            <*> fmap (fromMaybe "") (app ^. appRecipeEdit . editInstructions)
            <*> fmap parseNum (masks ^. editDuration)
            <*> masks ^. editYield
            <*> masks ^. editYieldUnit
            <*> fmap mtext (masks ^. editSource)
            <*> fmap mtext (masks ^. editURL)
    pure r

ingredientEditor :: GarlicApp -> Garlic (Event (Entity Ingredient))
ingredientEditor app = do
    let ni = app ^. appRecipeEdit . editNewIngredient

    ni ^. niClearAll `consume` ni ^. niClearClick
    ni ^. niClearAll `consume` ni ^. niOkClick

    new <- fetch newIngredient $ currentIngredient ni <@ ni ^. niOkClick
    stdout `consume` "TODO: ingredient already exists!" <$ filterE isNothing new
    
    pure (filterJust new)

currentIngredient :: GarlicNewIngredient -> Behavior Ingredient
currentIngredient editor = Ingredient
    <$> editor ^. niName
    <*> editor ^. niComment
    <*> editor ^. niUnit
    <*> (parseNum <$> editor ^. niAmount)
    <*> (parseNum <$> editor ^. niProtein)
    <*> (parseNum <$> editor ^. niCarbs)
    <*> (fmap parseNum . mtext <$> editor ^. niSugar)
    <*> (fmap parseNum . mtext <$> editor ^. niFibre)
    <*> (parseNum <$> editor ^. niFat)
    <*> (fmap parseNum . mtext <$> editor ^. niSatFat)
    <*> (fmap parseNum . mtext <$> editor ^. niPolyFat)
    <*> (fmap parseNum . mtext <$> editor ^. niMonoFat)
    <*> (fmap parseNum . mtext <$> editor ^. niTransFat)

ingredientList 
    :: GarlicIngredientList 
    -> Event [WeighedIngredient] 
    -> Event (Entity Ingredient)
    -> Garlic (Behavior [WeighedIngredient])
ingredientList ilist selected new = mdo
    let fromWI w = ( view wingrAmount w
                   , view wingrUnit w
                   , view (wingrIngr . to entityVal . to ingredientName) w
                   , view wingrOptional w )
        
        toWI (a,b,_,d) x = 
            let w = fromIngredient x
             in updWI (a,b,d) w

        updWI (a,b,c) = 
            set wingrOptional c . set wingrUnit b . set wingrAmount a

    -- Replace ingredient list on new selection
    ((() >$ ilist ^. ilClear) <> ilist ^. ilAppend) `consume` 
        map fromWI <$> selected

    -- Insert new ingredients
    ilist ^. ilAppend `consume` (pure . fromWI . fromIngredient) <$> new

    -- Events that can affect the managed list of ingredients
    let appendE  = (flip (++) . pure) . fromIngredient <$> new
        replaceE = const <$> selected
        deleteE  = delete' <$> ilist ^. ilDeleted
        changedE = (\(i,(a,b,_,c)) xs -> modify' i xs (updWI (a,b,c)))
               <$> whenE (not . null <$> is) (ilist ^. ilChanged)

    insertE <- do
        let x = ilist ^. ilInserted
            f (i,a) b = (i, toWI a b)
        xB <- stepper undefined $ x
        e  <- fetch ingredientByName $ view (_2 . _3) <$> x
        pure $ uncurry (flip . insert') <$> (f <$> xB <@> e)

    stdout `consume`  (("replace" <$ replaceE)
                   <:> ("insert"  <$ insertE)
                   <:> ("change"  <$ changedE))

    is <- accumB [] $ unions [ replaceE, deleteE, changedE, insertE, appendE ]
    pure is

modify' :: Int -> [a] -> (a -> a) -> [a]
modify' _ [] _ = []
modify' n xs f =
    let (l,x:r) = splitAt n xs
     in l ++ [f x] ++ r

insert' :: Int -> [a] -> a -> [a]
insert' 0 xs y = y : xs
insert' n xs y =
    case xs of
        []      -> pure y
        (x:xs') -> x : insert' (pred n) xs' y

delete' :: Int -> [a] -> [a]
delete' 0 xs = drop 1 xs
delete' n xs =
    case xs of
        [] -> []
        (x:xs') -> x : delete' (pred n) xs'
