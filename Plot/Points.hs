
module Plot.Points(linear, lines, clearCanvas, plot, fullPlot) where

import Prelude hiding (lines)
import HTk.Toplevel.HTk hiding (x, y)
import Plot.Structure
import Control.Arrow
import Control.Monad

-- Given two X coordinates and the Y coordinates they map to, this
-- function creates a linear interpolation (or extrapolation) based on
-- the mappings.
linear :: Fractional a => (a, a) -> (a, a) -> a -> a
linear (x1, x2) (y1, y2) x = m * (x - x1) + y1
    where m = (y2 - y1) / (x2 - x1)

-- This simply utility function returns a list of all subsequent pairs
-- of elements in its argument, so for instance it maps [1, 2, 3, 4]
-- to [(1, 2), (2, 3), (3, 4)].
lines :: [a] -> [(a, a)]
lines [] = []
lines (x:xs) = helper x xs
    where helper _ [] = []
          helper y (z:zs) = (y, z) : helper z zs

-- This convenience function deletes all of the contents of the
-- canvas, effectively erasing the last plot.
clearCanvas :: Canvas -> IO ()
clearCanvas cnvs = do
  tag <- createCanvasTag cnvs []
  addCanvasTag allItems tag
  destroy tag

-- This function produces a set of points to plot. Specifically, given
-- upper and lower bounds, a step value, and an expression, this
-- function produces the values obtained by plugging in each
-- subsequent point between the bounds, separated by the step value,
-- into the expression.
plot :: (Enum a, Floating a) => (a, a) -> a -> Expr a -> [(a, a)]
plot (a, b) delta expr = map (id &&& flip eval expr) [a, a + delta .. b]

-- Given an expression and a canvas, this function does the actual
-- work of plotting the expression on the canvas.
fullPlot :: Canvas -> Expr Double -> IO ()
fullPlot cnvs expr = do
  -- We first get the width and height of the canvas. These values are
  -- of type Distance. We will convert them to centimeters soon so we
  -- can more easily do arithmetic on them.
  (w, h) <- getSize cnvs
  -- We clear the canvas to eliminate any old plots.
  clearCanvas cnvs
  -- These functions use the linear interpolation function defined
  -- above to construct translations, which will take coordinates on
  -- our abstract coordinate grid and map them to coordinates on the
  -- physical canvas in the window.
  let bounds = (-3, 3)
      epsilon = 0.1
      translateX = cm . linear bounds (0, tocm w)
      translateY = cm . linear bounds (tocm h, 0)
      translate = translateX *** translateY
  -- Now, for each pair of subsequent points in the plot, we create a
  -- line on the canvas corresponding to that pair.
  forM_ (lines $ plot bounds epsilon expr) $ \(p1, p2) -> do
                       createLine cnvs [coord [translate p1, translate p2]]
