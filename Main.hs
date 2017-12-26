
module Main where

import HTk.Toplevel.HTk
import Plot.Parse
import Plot.Points
import Text.Parsec

clickEvent :: Entry String -> Canvas -> IO ()
clickEvent entry cnvs = do
  txt <- getValue entry
  case parse exprParser "(text-field)" $ filter (/= ' ') txt of
    Right expr -> fullPlot cnvs expr
    Left  err  -> print err

main :: IO ()
main = do
  mainwin <- initHTk [minSize (cm 15, cm 15), maxSize (cm 15, cm 15)]
  cnvs <- newCanvas mainwin [background "white", size (cm 15, cm 15)]
  frame <- newFrame mainwin []
  entry <- newEntry frame []
  btn <- newButton frame [text "Plot"]
  click <- clicked btn
  (ret, _) <- bindSimple entry (KeyRelease $ Just (KeySym "Return"))
  _ <- spawnEvent $ forever (click >>> clickEvent entry cnvs +> ret >>> clickEvent entry cnvs)
  pack frame [Fill X, Side AtBottom]
  pack entry [Fill X, Side AtLeft, Expand On]
  pack btn [Fill None, Side AtRight]
  pack cnvs [Fill Both, Side AtTop, Expand On]
  forceFocus entry
  finishHTk
