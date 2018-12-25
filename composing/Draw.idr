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
  drawLine : (win : Var) -> Line -> Col -> ST m () [win ::: Surface]

Draw IO where
  Surface = SDLSurface
  openWindow x y = 
    with ST do
      Just srf <- startSDL x y
        | Nothing => pure Nothing
      win <- new srf
      pure (Just win)
  closeWindow win = ?hole
  poll = ?Draw_rhs_4
  flip win = ?Draw_rhs_5
  filledRectangle win x y z = ?Draw_rhs_6
  drawLine win x y = ?Draw_rhs_7

-- Local Variables:
-- idris-load-packages: ("sdl" "contrib")
-- End:
