module Main

import Control.ST
import Control.ST.ImplicitCall

import Graphics.SDL

import Draw2

interface TurtleGraphics (m : Type -> Type) where
  Turtle : Type
  start : (Int, Int, Int) -> Col ->
          ST m (Maybe Var) [addIfJust Turtle]
  end : (t : Var) -> ST m () [remove t Turtle]
  
  fw : (t : Var) -> Int -> ST m () [t ::: Turtle]
  rt : (t : Var) -> Int -> ST m () [t ::: Turtle]
  col : (t : Var) -> Col -> ST m () [t ::: Turtle]
  -- penup : (t : Var) -> ST m () [t ::: Turtle]
  -- pendown : (t : Var) -> ST m () [t ::: Turtle]

  render : (t : Var) -> ST m () [t ::: Turtle]

Draw m => TurtleGraphics m where
  Turtle = Composite [
             Surface {m},
             State (Int, Int, Int),
             State Col,
             State (List Line)
           ] 
  start p col = do
     Just win <- openWindow 640 480
       | Nothing => pure Nothing
     filledRectangle win 0 0 640 480 black
     c <- new col
     -- ?hole     
     lines <- new [] 
     pos <- new p
     t <- new ()
     combine t [win, pos, c, lines]
     pure (Just t)
  end t = do
    [srf, pos, col, lines] <- split t
    closeWindow srf
    delete pos; delete col; delete lines
    delete t
  fw t d = do
      [srf, pos, col, lines] <- split t
      (x, y, a) <- read pos
      let x' = x + cast (cast d * (sin $ rad a))
      let y' = y + cast (cast d * (cos $ rad a))
      write pos (x', y', a)
      xs <- read lines
      c <- read col
      write lines (((x, y), (x', y'), c) :: xs)
      combine t [srf, pos, col, lines]
    where
      rad : Int -> Double
      rad g = cast g * pi / 180.0
  rt t angle = do
    [srf, pos, col, lines] <- split t
    (x, y, a) <- read pos
    write pos (x, y, (a + angle) `mod` 360)
    combine t [srf, pos, col, lines]
  col t c = do
    [srf, pos, col, lines] <- split t
    write col c
    combine t [srf, pos, col, lines]
  -- penup t = ?TurtleGraphics_rhs_7
  -- pendown t = ?TurtleGraphics_rhs_8
  render t = with ST do
      [srf, pos, col, lines] <- split t
      xs <- read lines
      filledRectangle srf 0 0 640 480 black
      drawAll srf xs      
      flip srf
      combine t [srf, pos, col, lines]      
      Just ev <- poll
        | Nothing => render t
      case ev of
        KeyUp _ => pure ()
        _       => render t
    where
      drawAll : (win : Var) -> List Line -> ST m () [win ::: Surface {m}]
      drawAll win [] = pure ()
      drawAll win (ln :: xs) = do drawLine win ln; drawAll win xs
    


turtle : (ConsoleIO m, TurtleGraphics m) => ST m () []
turtle = do
  Just t <- start (300, 200, 0) yellow
    | Nothing => pure ()
  let f = fw t
  let r = rt t
  let c = col t
  f 200; r 90
  c green; f 200; r 90
  c blue; f 200; r 90
  c red; f 200
  render t
  end t

main : IO ()
main = run turtle
  

-- Local Variables:
-- idris-load-packages: ("sdl" "contrib")
-- End:
