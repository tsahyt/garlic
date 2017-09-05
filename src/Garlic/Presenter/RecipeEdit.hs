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

    -- Ingredients
    ingredients <- ingredientList app

    -- Show Editor on Edit or Add Click, hide Edit Button and yield
    let click = app ^. appHeader . editClick
            <:> app ^. appHeader . addClick
     in do app ^. appRecipeEdit . showEditor `consume` click
           app ^. appHeader . yieldToggle `consume` click
           app ^. appHeader . editToggle `consume` click

    loadRecipe app (entityVal <$> selected)
    recipe <- currentRecipe app

    -- Recipe Entity, only Just when there is also a previous selection
    let recipeEntity = getCompose $ Entity
                   <$> Compose key 
                   <*> Compose (Just <$> recipe)
    
    -- Save on Store Click, or do nothing when there was no selection
    let storeE = filterJust (recipeEntity <@ app ^. appRecipeEdit . editStore)
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
          <$> storeE ]

revertDisplay :: GarlicApp -> Event a -> Garlic ()
revertDisplay app e = do
    app ^. appRecipeDisplay . showDisplay `consume` () <$ e
    app ^. appHeader . yieldToggle `consume` () <$ e
    app ^. appHeader . editToggle `consume` () <$ e

-- | Load recipe into mask on selection event
loadRecipe :: GarlicApp -> Event Recipe -> Garlic ()
loadRecipe app rcp = do
    let masks = app ^. appRecipeEdit . editMasks
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

ingredientList :: GarlicApp -> Garlic (Behavior [WeighedIngredient])
ingredientList app = do
    -- Update completion list when necessary
    ilist <- stepper [] =<< fetch completionList (app ^. appStartup)
    consume (app ^. appRecipeEdit . editReplaceIngCompl) =<< plainChanges ilist

    -- Shorthands
    let reg = app ^. appRecipeEdit . editRegIngredient
        fromWI WeighedIngredient{..} = 
            (_wingrAmount, _wingrUnit, ingredientName . entityVal $ _wingrIngr)

    -- New Ingredient Editor
    ingredientCreated <- ingredientEditor app

    -- Registration of new Ingredient, and showing it
    regE <- do 
        lastIngr <- stepper Nothing (Just <$> ingredientCreated)
        e <- fetch reg . fmap (fromWI . fromIngredient) 
           . filterJust =<< plainChanges lastIngr
        let f  = (\x y -> fmap (,y) x) <$> lastIngr
            e' = filterJust . apply f $ e
        pure e'

    app ^. appRecipeEdit ^. editAddIngredient `consume` snd <$> regE
    
    -- Maintaining Behavior of active ingredients
    (registered :: Behavior [WeighedIngredient]) <- mdo
        let addE   = flip (++) . return <$> regE
            mkDel  = unions 
                   . map (\(i,(_,r)) -> deleteIdx i <$ r ^. irDeleteClick) 
                   . zip [0..]
            change = unions [ addE, delE ]

        delE <- switchE $ mkDel <$> refs <@ change
        refs <- accumB [] change
        refsE <- plainChanges refs

        amounts <- switchB (pure []) $ 
            sequenceA . toListOf (traverse . _2 . irAmount) <$> refsE
        units <- switchB (pure []) $ 
            sequenceA . toListOf (traverse . _2 . irUnit) <$> refsE
        optionals <- switchB (pure []) $
            sequenceA . toListOf (traverse . _2 . irOptional) <$> refsE

        pure . getCompose $ WeighedIngredient
           <$> Compose amounts
           <*> Compose units
           <*> Compose optionals
           <*> Compose (fmap (map fst) refs)

    return registered

deleteIdx :: Int -> [a] -> [a]
deleteIdx 0 []     = []
deleteIdx 0 (_:xs) = xs
deleteIdx _ []     = []
deleteIdx n (x:xs) = x : deleteIdx (pred n) xs

ingredientEditor :: GarlicApp -> Garlic (Event (Entity Ingredient))
ingredientEditor app = do
    let ni = app ^. appRecipeEdit . editNewIngredient

    ni ^. niClearAll `consume` ni ^. niClearClick
    ni ^. niClearAll `consume` ni ^. niOkClick

    fetch newIngredient $ currentIngredient ni <@ ni ^. niOkClick

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
