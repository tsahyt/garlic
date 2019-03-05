{-# LANGUAGE GADTs #-}
module Garlic.Model.CSV
(
    importCSV
)
where

import Control.Monad.Trans
import Database.Persist
import Database.Persist.Sql
import Data.Conduit hiding (Consumer)
import Data.Conduit.Binary
import Data.CSV.Conduit
import Data.Text (Text, unpack)

import Garlic.Model
import Garlic.Types (mtext, parseNum, Consumer, dbConsumer)

import qualified Data.Conduit.List as C

toIngredient :: Monad m => ConduitT (Row Text) Ingredient m ()
toIngredient = C.mapMaybe $ \x -> do
    [ name, comment, unit, amount, protein, carbs, sugar, fibre
          , fat, satFat, polyFat, monoFat, transFat, sodium, cholesterol ] 
          <- pure x

    pure Ingredient 
        { ingredientName        = name
        , ingredientComment     = comment
        , ingredientBasicUnit   = read . unpack $ unit
        , ingredientBasicAmount = parseNum amount
        , ingredientProtein     = parseNum protein
        , ingredientCarbs       = parseNum carbs
        , ingredientSugar       = fmap parseNum . mtext $ sugar
        , ingredientFibre       = fmap parseNum . mtext $ fibre
        , ingredientFat         = parseNum fat
        , ingredientSatFat      = fmap parseNum . mtext $ satFat
        , ingredientPolyFat     = fmap parseNum . mtext $ polyFat
        , ingredientMonoFat     = fmap parseNum . mtext $ monoFat
        , ingredientTransFat    = fmap parseNum . mtext $ transFat
        , ingredientSodium      = fmap parseNum . mtext $ sodium
        , ingredientCholesterol = fmap parseNum . mtext $ cholesterol
        }

importCSV' :: FilePath -> SqlPersistT IO ()
importCSV' fp = runConduitRes $ 
        sourceFile fp 
     .| intoCSV defCSVSettings 
     .| toIngredient 
     .| C.mapM_ (lift . insert_)

importCSV :: Consumer FilePath
importCSV = dbConsumer importCSV'
