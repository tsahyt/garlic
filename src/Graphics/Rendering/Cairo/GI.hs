module Graphics.Rendering.Cairo.GI
    ( renderWithContext
    ) where

import Control.Monad.Reader (runReaderT)
import Data.GI.Base
import Foreign.Ptr (castPtr)
import qualified GI.Cairo as GI.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r =
    withManagedPtr ct $ \p -> runReaderT (runRender r) (Cairo (castPtr p))
