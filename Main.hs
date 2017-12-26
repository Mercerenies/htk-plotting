
module Main where

import HTk.Toplevel.HTk
import Plot.Parse
import Plot.Points
import Text.Parsec

main :: IO ()
main = do
  -- We initialize HTk, which gives us a main window unless we
  -- explicitly request that one not be created. Here, we set the
  -- minimum and maximum sizes for the main window to be the same,
  -- effectively making the window non-resizable. We also set the
  -- contents of its titlebar.
  mainwin <- initHTk [minSize (cm 15, cm 15), maxSize (cm 15, cm 15), text "Plotting"]
  -- Our window will consist of a canvas at the top, as well as an
  -- entry field and a button at the bottom. The geometry manager
  -- we're using here is the packing geometry manager, which tends to
  -- be very simple at the cost of some amount of flexibility.
  cnvs <- newCanvas mainwin [background "white", size (cm 15, cm 15)]
  frame <- newFrame mainwin []
  entry <- newEntry frame [value "x * (x - 1) * (x + 1)"]
  btn <- newButton frame [text "Plot"]
  -- This generates a click event for the button. This event will
  -- "fire" whenever the button is clicked.
  click <- clicked btn
  -- The clickEvent function below requires access to the entry field
  -- and the canvas, so we'll parameterize the function here and store
  -- the resulting IO object.
  let event = clickEvent entry cnvs
  -- We want to react to the return key being pressed when the entry
  -- field has focus, so we create a binding here. The second return
  -- value, which is discarded in this example, would be an IO action
  -- that, when executed, eliminates the binding.
  (ret, _) <- bindSimple entry (KeyRelease $ Just (KeySym "Return"))
  -- Now we spawn an asynchronous thread which will perform the event
  -- handling. The contents of the line after the dollar sign read
  -- "forever (for the lifetime of the thread), if the event click
  -- occurs then execute event, and if the event ret occurs then
  -- execute event".
  _ <- spawnEvent $ forever (click >>> event +> ret >>> event)
  -- Now we use a geometry manager to position the elements in the
  -- window. The packing geometry manager is capable of only simply
  -- placements, so we use a frame to better control our placement.
  pack frame [Fill X, Side AtBottom]
  pack entry [Fill X, Side AtLeft, Expand On]
  pack btn [Fill None, Side AtRight]
  pack cnvs [Fill Both, Side AtTop, Expand On]
  -- The focus of the window will start on the entry field, so we can
  -- start typing as soon as the program runs.
  forceFocus entry
  -- Since the entry field starts out with an initial value containing
  -- a sample function, we want to go ahead and plot the function now,
  -- so simulate a click of the "Plot" button.
  event
  -- This function forfeits control to the Tk event loop. The program
  -- will continue running until forcefully exited or until we hit the
  -- exit button on the main window.
  finishHTk

-- This is going to be the event that runs when the "Plot" button is
-- clicked or the return key is pressed. It will update the canvas
-- with the new plot.
clickEvent :: Entry String -> Canvas -> IO ()
clickEvent entry cnvs = do
  -- We get the text from the entry field and attempt to parse it as
  -- an expression. If successful, we will plot the function. If
  -- unsuccessful, we will print an error message to the console.
  txt <- getValue entry
  case parse exprParser "(text-field)" $ filter (/= ' ') txt of
    Right expr -> fullPlot cnvs expr
    Left  err  -> print err
