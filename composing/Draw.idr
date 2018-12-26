module Draw

import Control.ST
import Graphics.SDL

%access public export

data Col = MkCol Int Int Int Int

black : Col
black = MkCol 0 0 0 255

white : Col
white = MkCol 255 255 255 255

red : Col
red = MkCol 255 0 0 255

green : Col
green = MkCol 0 255 0 255

blue : Col
blue = MkCol 0 0 255 255

yellow : Col 
yellow = MkCol 255 255 0 255

Line : Type
Line = ((Int, Int), (Int, Int), Col)

interface Draw (m : Type -> Type) where
  Surface : Type
  openWindow : Int -> Int -> ST m (Maybe Var) [addIfJust Surface]
  closeWindow : (win : Var) -> ST m () [remove win Surface]
  poll : ST m (Maybe Event) []
  flip : (win : Var) -> ST m () [win ::: Surface]
  filledRectangle : (win : Var) -> (Int, Int) -> (Int, Int) -> Col -> 
                    ST m () [win ::: Surface]
  drawLine : (win : Var) -> Line -> ST m () [win ::: Surface]

Draw IO where
  Surface = State SDLSurface
  openWindow x y = 
    with ST do
      Just srf <- lift $ startSDL x y
        | Nothing => pure Nothing
      win <- new srf
      pure (Just win)
  closeWindow win = 
    with ST do
      lift endSDL
      delete win
  poll = with ST do
    Just e <- lift pollEvent
      | Nothing => pure Nothing
    pure (Just e)
  flip win = with ST do
    srf <- read win
    lift $ flipBuffers srf
  filledRectangle win (x, y) (w, h) (MkCol r g b a) = with ST do
    srf <- read win
    lift $ filledRect srf x y w h r g b a
  drawLine win ((x, y), (ex, ey), MkCol r g b a) = with ST do
    srf <- read win
    lift $ drawLine srf x y ex ey r g b a
    
    

-- Local Variables:
-- idris-load-packages: ("sdl" "contrib")
-- End:
