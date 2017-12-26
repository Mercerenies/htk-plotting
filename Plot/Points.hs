
module Plot.Points(linear, lines, clearCanvas, plot, fullPlot) where

import Prelude hiding (lines)
import HTk.Toplevel.HTk hiding (x, y)
import Plot.Structure
import Control.Arrow
import Control.Monad

linear :: Fractional a => (a, a) -> (a, a) -> a -> a
linear (x1, x2) (y1, y2) x = m * (x - x1) + y1
    where m = (y2 - y1) / (x2 - x1)

lines :: [a] -> [(a, a)]
lines [] = []
lines (x:xs) = helper x xs
    where helper _ [] = []
          helper y (z:zs) = (y, z) : helper z zs

clearCanvas :: Canvas -> IO ()
clearCanvas cnvs = do
  tag <- createCanvasTag cnvs []
  addCanvasTag allItems tag
  destroy tag

plot :: (Enum a, Floating a) => (a, a) -> a -> Expr a -> [(a, a)]
plot (a, b) delta expr = map (id &&& flip eval expr) [a, a + delta .. b]

fullPlot :: Canvas -> Expr Double -> IO ()
fullPlot cnvs expr = do
  (w, h) <- getSize cnvs
  clearCanvas cnvs
  let bounds = (-3, 3)
      epsilon = 0.1
      translateX = cm . linear bounds (0, tocm w)
      translateY = cm . linear bounds (tocm h, 0)
      translate = translateX *** translateY
  forM_ (lines $ plot bounds epsilon expr) $ \(p1, p2) -> do
                       createLine cnvs [coord [translate p1, translate p2]]
