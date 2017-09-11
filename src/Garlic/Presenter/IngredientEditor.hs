module Garlic.Presenter.IngredientEditor
(
)
where

import Control.Lens
import Garlic.Types
import Reactive.Banana

import Garlic.View.IngredientEditor

ingredientEditorP
    :: GarlicIngredientEditor
    -> Garlic ()
ingredientEditorP editor = pure ()
