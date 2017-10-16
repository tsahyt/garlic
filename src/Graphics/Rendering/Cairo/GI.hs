module Graphics.Rendering.Cairo.GI
    ( renderWithContext
    , dimensions
    ) where
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class
import Data.GI.Base
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

import qualified GI.Cairo
import qualified GI.Gtk as Gtk

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r =
    withManagedPtr ct $ \p -> runReaderT (runRender r) (Cairo (castPtr p))

dimensions :: (MonadIO m, Num a) => Gtk.DrawingArea -> m (a, a)
dimensions da = do
    w <- fromIntegral <$> Gtk.widgetGetAllocatedWidth da
    h <- fromIntegral <$> Gtk.widgetGetAllocatedHeight da
    pure (w, h)
