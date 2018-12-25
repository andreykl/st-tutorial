module Main

import Control.ST
import Control.ST.ImplicitCall
import Graphics.SDL

data Col = MkCol Int Int Int Int

black : Col
black = MkCol 0 0 0 255

red : Col
red = MkCol 255 0 0 255

green : Col
green = MkCol 0 255 0 255

blue : Col
blue = MkCol 0 0 255 255

yellow : Col
yellow = MkCol 255 255 0 255

white : Col
white = MkCol 255 255 255 255

interface Draw (m : Type -> Type) where
  Surface : Type
  initWindow : Int -> Int -> ST m (Maybe Var) [addIfJust Surface]
  closeWindow : (win : Var) -> ST m () [remove win Surface]
  poll : ST m (Maybe Event) []
  flip : (win : Var) -> ST m () [win ::: Surface]
  filledRectangle : (win : Var) -> (Int, Int) -> (Int, Int) -> Col -> ST m () [win ::: Surface]
  drawLine : (win : Var) -> (Int, Int) -> (Int, Int) -> Col -> ST m () [win ::: Surface]

Draw IO where
  Surface = State SDLSurface
  initWindow x y = do Just srf <- lift (startSDL x y)
                        | pure Nothing
                      var <- new srf
                      pure (Just var)
  closeWindow win = do lift endSDL
                       delete win
  poll = lift pollEvent
  flip win = do srf <- read win
                lift (flipBuffers srf)
  
  filledRectangle win (x, y) (ex, ey) (MkCol r g b a) =
    do srf <- read win
       lift $ filledRect srf x y ex ey r g b a
  drawLine win (x, y) (ex, ey) (MkCol r g b a) =
    do srf <- read win
       lift $ drawLine srf x y ex ey r g b a


-- render : Draw m => (win : Var) -> ST m () [win ::: Surface {m}]
-- render win = do filledRectangle win (0, 0) (640, 480) black
--                 drawLine win (100, 100) (200, 200) red
--                 flip win

-- loop : Draw m => (win : Var) -> ST m () [win ::: Surface {m}]
-- loop win = do render win
--               Just AppQuit <- call poll
--                 | _ => loop win
--               pure ()

-- drawMain : (ConsoleIO m, Draw m) => ST m () []
-- drawMain = do Just win <- initWindow 640 480
--                 | Nothing => putStrLn "Can't open a window"
--               loop win
--               closeWindow win

interface TurtleGraphics (m : Type -> Type) where
  Turtle : Type

  start : Int -> Int -> ST m (Maybe Var) [addIfJust Turtle]
  end : (t : Var) -> ST m () [Remove t Turtle]

  fd : (t : Var) -> Int -> ST m () [t ::: Turtle]
  rt : (t : Var) -> Int -> ST m () [t ::: Turtle]

  penup : (t : Var) -> ST m () [t ::: Turtle]
  penddown : (t : Var) -> ST m () [t ::: Turtle]
  col : (t : Var) -> Col -> ST m () [t ::: Turtle]
  
  render : (t : Var) -> ST m () [t ::: Turtle]


turtle : (ConsoleIO m, TurtleGraphics m) => ST m () []
turtle = with ST do
  Just t <- start 640 480
    | Nothing => putStrLn "Can't make a turtle"
  col t yellow
  fd t 100; rt t 90
  col t green
  fd t 100; rt t 90
  col t red
  fd t 100; rt t 90
  col t blue
  fd t 100; rt t 90
  render t
  end t


Line : Type
Line = ((Int, Int), (Int, Int), Col)

Draw m => TurtleGraphics m where
  Turtle = Composite [
                     Surface {m},
                     State Col,
                     State (Int, Int, Int, Bool),
                     State (List Line)
           ]
  start x y = with ST do
                Just srf <- initWindow x y
                  | Nothing => pure Nothing
                col <- new white
                pos <- new (320, 200, 0, True)
                lines <- new []
                turtle <- new ()
                combine turtle [srf, col, pos, lines]
                pure (Just turtle)
  end t = with ST do
            [s, c, p, ls] <- split t
            call $ closeWindow s
            delete c
            delete p
            delete ls
            delete t

  fd t dx = with ST do
      [srf, col, pos, lines] <- split t
      (x, y, d, p) <- read pos
      let x' = cast x + cast dx * sin (rad d)
      let y' = cast y + cast dx * cos (rad d)
      c <- read col
      ls <- read lines
      write lines (if p 
                   then
                     ((x, y), (cast x', cast y'), c) :: ls 
                   else
                     ls)
      write pos (cast x', cast y', d, p)
      combine t [srf, col, pos, lines]
    where
      rad : Int -> Double
      rad x = (cast x * pi) / 180.0
    
  rt t angle = with ST do
    [srf, col, pos, lines] <- split t
    (x, y, d, p) <- read pos
    write pos (x, y, (d + angle) `mod` 360, p)
    combine t [srf, col, pos, lines]
  penup t = with ST do
    [srf, col, pos, lines] <- split t
    (x, y, d, _) <- read pos
    write pos (x, y, d, False)
    combine t [srf, col, pos, lines]
  penddown t = with ST do
    [srf, col, pos, lines] <- split t
    (x, y, d, _) <- read pos
    write pos (x, y, d, True)
    combine t [srf, col, pos, lines]
  col t c = with ST do
    [srf, col, pos, lines] <- split t
    write col c
    combine t [srf, col, pos, lines]
  render t = with ST do
      [srf, col, pos, lines] <- split t
      filledRectangle srf (0, 0) (640, 480) black      
      drawAll srf !(read lines)
      flip srf
      combine t [srf, col, pos, lines]
      Just ev <- poll
        | Nothing => render t
      case ev of
        KeyUp _ => pure ()
        _       => render t

    where drawAll : (srf : Var) -> List Line -> ST m () [srf ::: Surface {m}]
          drawAll srf [] = pure ()
          drawAll srf ((start, end, col) :: xs) = 
            do drawLine srf start end col
               drawAll srf xs



main : IO ()
main = run turtle


-- Local Variables:
-- idris-load-packages: ("contrib" "sdl")
-- End:
