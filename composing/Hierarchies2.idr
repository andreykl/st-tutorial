module Main

import Control.ST
import Control.ST.ImplicitCall

import Graphics.SDL

import Draw

interface TurtleGraphics (m : Type -> Type)  where
  Turtle : Type
  
  start : (Int, Int) -> Col -> ST m (Maybe Var) [addIfJust Turtle]
  end : (t : Var) -> ST m () [remove t Turtle]
  
  fw : (t : Var) -> Int -> ST m () [t ::: Turtle]
  rt : (t : Var) -> Int -> ST m () [t ::: Turtle]
  col : (t : Var) -> Col -> ST m () [t ::: Turtle]

  penup : (t : Var) -> ST m () [t ::: Turtle]
  pendown : (t : Var) -> ST m () [t ::: Turtle]
  
  render : (t : Var) -> ST m () [t ::: Turtle]


Draw m => TurtleGraphics m where
  Turtle = Composite [Surface {m}, State (Int, Int, Int, Bool),
                      State Col, State (List Line)]
  start (x, y) c = with ST do 
    Just srf <- openWindow x y
      | Nothing => pure Nothing
    pos <- new (x, y, 0, True)
    col <- new c
    lines <- new []
    t <- new ()
    combine t [srf, pos, col, lines]
    pure (Just t)
  end t = do
    [srf, pos, col, lines] <- split t
    closeWindow srf
    delete pos
    delete col
    delete lines
    delete t
  fw t d = do
      [srf, pos, col, lines] <- split t
      (x, y, a, p) <- read pos
      write pos (x + cast (cast d * sin (rad a)), 
                 y + cast (cast d * cos (rad a)), a, p)
      combine t [srf, pos, col, lines]
    where
      rad : Int -> Double
      rad g = (cast g * pi) / 180.0
  rt t a = do
    [srf, pos, col, lines] <- split t
    (x, y, a', p) <- read pos
    write pos (x, y, (a + a') `mod` 360, p)
    combine t [srf, pos, col, lines]
  col t c = do
    [srf, pos, col, lines] <- split t
    write col c
    combine t [srf, pos, col, lines]
  penup t = do
    [srf, pos, col, lines] <- split t
    (x, y, a, _) <- read pos
    write pos (x, y, a, False)
    combine t [srf, pos, col, lines]
  pendown t = do
    [srf, pos, col, lines] <- split t
    (x, y, a, _) <- read pos
    write pos (x, y, a, True)
    combine t [srf, pos, col, lines]
  render t = do
      [srf, pos, col, lines] <- split t
      filledRectangle srf (0, 0) (640, 480) black
      drawAll srf !(read lines)
      flip srf
      combine t [srf, pos, col, lines]
      Just ev <- poll
        | Nothing => render t
      case ev of
        KeyUp _ => pure ()
        _       => render t
    where
      drawAll : (srf : Var) -> List Line -> ST m () [srf ::: Surface {m}]
    


-- Local Variables:
-- idris-load-packages: ("sdl" "contrib")
-- End:
