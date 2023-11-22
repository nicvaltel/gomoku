module GameProcessor where


import Prelude
import Data.Tuple(Tuple(..))
import Data.Maybe (Maybe(..))

type MouseX = Int
type MouseY = Int
type CanvasWidth = Int
type CanvasHeight = Int
type GridSize = Int
type BoardSize = Int
type BorderSize = Int

findNearestToMouseCoord :: MouseX -> MouseY -> CanvasWidth -> CanvasHeight -> GridSize -> BoardSize -> BorderSize -> Maybe (Tuple MouseX MouseY)
findNearestToMouseCoord mouseX mouseY canvasWidth canvasHeight gridSize boardSize borderSize
    | mouseX < 0 || mouseY < 0 || mouseX > canvasWidth || mouseY > canvasHeight = Nothing
    | otherwise =
        let row = (mouseX - borderSize) / gridSize
            col = (mouseY - borderSize) / gridSize
         in if (row >= 0 && col >= 0 && row < boardSize && col < boardSize) 
                then Just (Tuple row col)
                else Nothing
