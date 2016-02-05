{-
  Sample "Doodling" example in Elm
  Pedro Vasconcelos, 2016
  pbv@dcc.fc.up.pt
-}
module Doodle where 

import Mouse
import Signal
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color 
import Window
import List


--
-- Model
--
type alias Point = (Int,Int)  -- ^ x,y coordinates

type alias Model 
  = { current : List Point         -- ^ points in current path
    , previous : List (List Point) -- ^ list of completed paths
    }

-- initial model 
initial = { current = [], previous = []  }

update : (Bool, Point) -> Model -> Model
update (mouseDown, pt) model
  = if mouseDown then
      { model | current = pt::model.current }
    else if List.isEmpty model.current then
           model
         else
           { model | current = [], previous = model.current::model.previous }
      
--
-- View 
--
view : (Int,Int) -> Model -> Element
view (w,h) model = 
  let 
      shift (x,y) = (toFloat x - toFloat w/2, toFloat h/2- toFloat y)
      segment pts = path (List.map shift pts)
  in collage w h <| 
       (traced (dashed Color.red) <| segment model.current) ::
       List.map (traced (solid Color.black) << segment) model.previous


                    
--
-- putting it all together
--
main : Signal Element
main =
  Signal.map2 view Window.dimensions doodle

doodle : Signal Model
doodle 
  = Signal.foldp update initial (Signal.map2 (,) Mouse.isDown Mouse.position) 
