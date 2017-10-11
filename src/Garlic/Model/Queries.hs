{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Garlic.Model.Queries
(
    WeighedIngredient (..),
    fromIngredient,
    wingrAmount,
    wingrUnit,
    wingrIngr,
    wingrOptional,
    wingrDisp,
    wingrGroup,

    -- * Recipes
    recipes,
    ingredientsFor,
    newRecipe,
    updateRecipe,
    deleteRecipe,

    -- * Recipes for FoodLog
    recipeShort,

    -- * Ingredients
    allIngredientNames,
    ingredientByName,
    ingredientsByName,
    newIngredient,
    deleteIngredient,
    updateIngredient,

    -- * Weight Measurements
    getWeightMeasurements,
    addWeightMeasurement,
    deleteWeightMeasurement,

    -- * Goals
    getGoals,
    addGoal,
    deleteGoal
)
where

import Control.Lens.TH
import Garlic.Data.Units
import Garlic.Model
import Garlic.Data.Meal
import Garlic.Types
import Data.List (sortBy)
import Data.Foldable
import Data.Time
import Data.Ord
import Data.Maybe (catMaybes)
import Database.Esqueleto
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Map (Map)
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Text as T

import qualified Database.Persist as P

-- | Weighted ingredients are ingredients with an amount and a unit.
data WeighedIngredient = WeighedIngredient
    { _wingrAmount   :: Double
    , _wingrUnit     :: Unit
    , _wingrOptional :: Bool
    , _wingrDisp     :: Maybe Text
    , _wingrGroup    :: Maybe Text
    , _wingrIngr     :: Entity Ingredient
    }
    deriving Show

fromIngredient :: Entity Ingredient -> WeighedIngredient
fromIngredient e@(Entity _ v) = 
    WeighedIngredient 
        (ingredientBasicAmount v)
        (ingredientBasicUnit v)
        False
        Nothing
        Nothing
        e

makeLenses ''WeighedIngredient

-- | Fetcher to obtain all recipes with their associated weighted ingredients
-- from the database. The query is done in two steps, once for all recipes, then
-- as a loop over those recipes, rather than joining and then collapsing by
-- recipe.
recipes :: Fetcher Text (Seq (Entity Recipe))
recipes = dbFetcher $ \str -> do
    let str' = flip T.snoc '%' . T.cons '%' $ str
    rs <- select $ 
          from $ \r -> do
              where_ (r ^. RecipeName `like` val str')
              return r
    pure . S.fromList $ rs

-- | Select all weighted ingredients for some recipe
ingredientsFor :: Fetcher (Key Recipe) [WeighedIngredient]
ingredientsFor = dbFetcher ingredientsFor'

ingredientsFor' :: Key Recipe -> SqlPersistT IO [WeighedIngredient]
ingredientsFor' recipe = do
    xs <- select $
            from $ \(h,i) -> do
                where_ (h ^. RecipeHasRecipe ==. val recipe
                    &&. i ^. IngredientId ==. h ^. RecipeHasIngredient)

                return (h, i)

    pure $
        map
            (\(Entity _ (RecipeHas {..}), i) ->
                 WeighedIngredient
                     recipeHasAmount
                     recipeHasUnit
                     recipeHasOptional
                     recipeHasDisplay
                     recipeHasGroup
                     i)
            xs

recipeShort :: Fetcher (Meal, Text) (Maybe (Meal, Text, Double, [WeighedIngredient]))
recipeShort = dbFetcher $ \(m,t) -> do
    x <- P.selectFirst [ RecipeName P.==. t ] []
    case x of
        Nothing -> pure Nothing
        Just x' -> do
            ws <- ingredientsFor' (entityKey x')
            pure $ Just (m,t,recipeYield . entityVal $ x', ws)

-- | Select all ingredient names in the DB
allIngredientNames :: Fetcher () [Text]
allIngredientNames = dbFetcher $ \_ ->
    map (ingredientName . entityVal) <$> P.selectList [] []

ingredientByName :: Fetcher Text (Entity Ingredient)
ingredientByName = filterMaybe . dbFetcher $ \name ->
    P.selectFirst [ IngredientName P.==. name ] []

ingredientsByName :: Fetcher [Text] [Entity Ingredient]
ingredientsByName = dbFetcher $ \names -> do
    xs <- mapM (\n -> P.selectFirst [ IngredientName P.==. n ] []) names
    pure $ catMaybes xs

updateRecipe :: Consumer (Entity Recipe, [WeighedIngredient])
updateRecipe = dbConsumer $ \(Entity k r, is) -> do
    -- update the recipe entry
    P.repsert k r

    -- remove all old associations to ingredients
    delete $
        from $ \h ->
        where_ (h ^. RecipeHasRecipe ==. val k)

    -- insert new ones
    P.insertMany_ $
        [ RecipeHas k (entityKey _wingrIngr) _wingrAmount 
                    _wingrUnit _wingrOptional _wingrDisp _wingrGroup
        | WeighedIngredient{..} <- is ]

newRecipe :: Fetcher () (Entity Recipe)
newRecipe = dbFetcher $ \_ ->
    P.insertEntity $ 
        Recipe "New Recipe" "Cuisine" 0 "" 0 1 "Serving" Nothing Nothing

deleteRecipe :: Consumer (Key Recipe)
deleteRecipe = dbConsumer $ \k -> do
    delete $
        from $ \h ->
            where_ (h ^. RecipeHasRecipe ==. val k)
    P.delete k

newIngredient :: Fetcher Ingredient (Maybe (Entity Ingredient))
newIngredient = dbFetcher $ \i -> do
    x <- P.selectFirst [ IngredientName P.==. ingredientName i ] []
    case x of
        Just _  -> pure Nothing
        Nothing -> Just <$> P.insertEntity i

deleteIngredient :: Consumer (Key Ingredient)
deleteIngredient = dbConsumer $ \k -> do
    delete $
        from $ \h ->
            where_ (h ^. RecipeHasIngredient ==. val k)
    P.delete k

updateIngredient :: Consumer (Key Ingredient, Ingredient)
updateIngredient = dbConsumer $ \(k,i) -> P.repsert k i

getWeightMeasurements :: Fetcher UTCTime [Entity WeightMeasurement]
getWeightMeasurements =
    dbFetcher $ \l ->
        sortBy (comparing (weightMeasurementTimestamp . entityVal)) <$>
        P.selectList [WeightMeasurementTimestamp P.>=. l] []

addWeightMeasurement :: Consumer WeightMeasurement
addWeightMeasurement =
    dbConsumer $ \m -> do
        x <-
            P.selectFirst
                [WeightMeasurementTimestamp P.==. weightMeasurementTimestamp m]
                []
        case x of
            Just e -> P.replace (entityKey e) m
            Nothing -> P.insert_ m

deleteWeightMeasurement :: Consumer UTCTime
deleteWeightMeasurement =
    dbConsumer $ \t -> do P.deleteWhere [WeightMeasurementTimestamp P.==. t]

mapBy :: (Ord b, Foldable t) => (a -> b) -> t a -> Map b a
mapBy f = foldl' (\m x -> M.insert (f x) x m) M.empty

getGoals :: Fetcher () (Map Day Goal)
getGoals =
    dbFetcher $ \_ -> do
        xs <- P.selectList [] []
        pure $ mapBy (utctDay . goalTimestamp) (map entityVal xs)

addGoal :: Consumer Goal
addGoal =
    dbConsumer $ \g -> do
        x <-
            P.selectFirst
                [GoalTimestamp P.==. goalTimestamp g]
                []
        case x of
            Just e -> P.replace (entityKey e) g
            Nothing -> P.insert_ g

deleteGoal :: Consumer UTCTime
deleteGoal =
    dbConsumer $ \t -> do P.deleteWhere [GoalTimestamp P.==. t]
