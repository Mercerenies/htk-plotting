
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
  mainwin <- initHTk []
  cnvs <- newCanvas mainwin [background "white"]
  frame <- newFrame mainwin []
  entry <- newEntry frame []
  btn <- newButton frame [text "Plot"]
  click <- clicked btn
  (ret, _) <- bindSimple entry (KeyRelease $ Just (KeySym "Return"))
  _ <- spawnEvent $ forever (click >>> clickEvent entry cnvs +> ret >>> clickEvent entry cnvs)
  pack cnvs [Fill Both, Side AtTop, Expand On]
  pack frame [Fill X, Side AtBottom]
  pack entry [Fill X, Side AtLeft, Expand On]
  pack btn [Fill None, Side AtRight]
  forceFocus entry
  finishHTk
