module Intro

import Control.ST
import Data.Vect

increment : (x : Var) -> STrans m () [x ::: State Integer]
                                     (const [x ::: State Integer])
                                     
increment x = do num <- read x
                 write x (num + 1)

makeAndIncrement : Integer -> STrans m Integer [] (const [])
makeAndIncrement init = do var <- new init
                           increment var
                           x <- read var
                           delete var
                           pure x

-- data ConsoleIO1 : (m : Type -> Type) -> Type where
--   PutStr : String -> STrans m () res (const res) -> ConsoleIO1 m
--   GetStr : STrans m String res (const res) -> ConsoleIO1 m

-- interface ConsoleIO (m : Type -> Type) where
--   putStr : String -> STrans m () res (const res)
--   getStr : STrans m String res (const res)

-- Intro.ConsoleIO IO where
--   putStr str = lift $ putStr str
--   getStr = lift $ getLine


-- ioMakeAndIncrement : ConsoleIO io => STrans io () [] (const [])
-- ioMakeAndIncrement
--    = do putStr "Enter a number: "
--         init <- getStr
--         var <- new (cast init)
--         putStrLn ("var = " ++ show !(read var))
--         increment var
--         putStrLn ("var = " ++ show !(read var))
--         delete var

addElement : (vec : Var) -> 
             (item : a)  -> 
             STrans m () [vec ::: State (Vect n a)]
                         (const [vec ::: State (Vect (S n) a)])
addElement vec item = update vec (item::)
-- addElement vec item = do xs <- read vec
--                          write vec (item :: xs)

ioMakeAndIncrement : ConsoleIO io => STrans io () [] (const [])
ioMakeAndIncrement = do putStr "enter a number: "
                        s <- getStr
                        var <- new (cast s)
                        putStrLn ("var = " ++ show !(read var))
                        increment var
                        putStrLn ("var = " ++ show !(read var))
                        delete var

readAndAdd : ConsoleIO io => (vec : Var) ->
                             STrans io Bool [vec ::: State (Vect n Integer)]
                             (\res => if res 
                                      then [vec ::: State (Vect (S n) Integer)]
                                      else [vec ::: State (Vect n Integer)])
readAndAdd vec = do
  putStr "Enter a digit: "
  s <- getStr
  case unpack s of
    [c] => if isDigit c
           then do
             update vec ((cast $ ord c - ord '0') ::)
             pure True
           else
             pure False
    _   => pure False


-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
