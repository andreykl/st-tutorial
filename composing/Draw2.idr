module Draw2

import Control.ST
import Control.ST.ImplicitCall
import Graphics.SDL

%access public export

data Col = MkCol Int Int Int Int

black : Col 
black = MkCol 0 0 0 255

red : Col 
red = MkCol 255 0 0 255

green : Col 
green = MkCol 0 255 0 255

blue : Col 
blue = MkCol 0 0 255 255

cyan : Col 
cyan = MkCol 0 255 255 255

magenta : Col 
magenta = MkCol 255 0 255 255

yellow : Col 
yellow = MkCol 255 255 0 255

white : Col 
white = MkCol 255 255 255 255

Line : Type
Line = ((Int, Int), (Int, Int), Col)

interface Draw (m : Type -> Type) where
  Surface : Type
  openWindow : Int -> Int ->
               ST m (Maybe Var) [addIfJust Surface]
  closeWindow : (win : Var) -> ST m () [remove win Surface]
  filledRectangle : (win : Var) ->
                    Int -> Int -> Int -> Int -> Col ->
                    ST m () [win ::: Surface]
  drawLine : (win : Var) -> Line -> ST m () [win ::: Surface]
  flip : (win : Var) -> ST m () [win ::: Surface]
  poll : ST m (Maybe Event) []

Draw IO where
  Surface = State SDLSurface
  openWindow width height = do
    Just srf <- lift $ startSDL width height
      | Nothing => pure Nothing
    win <- new srf
    pure (Just win)
  closeWindow win = do lift endSDL; delete win
  filledRectangle win x y w h (MkCol r g b a) = do
    srf <- read win
    lift $ filledRect srf x y w h r g b a
  drawLine win ((x1, y1), (x2, y2), (MkCol r g b a)) = do
    srf <- read win
    lift $ drawLine srf x1 y1 x2 y2 r g b a
  flip win = do
    srf <- read win
    lift $ flipBuffers srf
  poll = lift pollEvent

-- Local Variables:
-- idris-load-packages: ("sdl" "contrib")
-- End:
