module Main

import Control.ST
import Control.ST.ImplicitCall

data Access = LoggedIn | LoggedOut

data LoginResult = LoginOK | BadPassword

interface DataStore (m : Type -> Type) where
  Store : Access -> Type
  connect : ST m Var [add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () [remove store (Store LoggedOut)]
  login : (store : Var) -> 
          ST m LoginResult
            [store ::: Store LoggedOut :-> 
               (\res => Store (case res of
                                 LoginOK => LoggedIn
                                 BadPassword => LoggedOut))]
  logout : (store : Var) -> 
           ST m () [store ::: Store LoggedIn :-> Store LoggedOut]
  readSecret : (store : Var) -> ST m String [store ::: Store LoggedIn]
          

DataStore IO where
  Store x = State String
  connect = do store <- new "These are data under password protection."
               pure store
  disconnect store = do delete store; pure ()
  login store = do
    putStr "enter password: "
    pass <- getStr
    if pass == "onemorepass"
      then pure LoginOK
      else pure BadPassword
  logout store = pure ()
  readSecret store = read store

getData : (ConsoleIO m, DataStore m) => (store, failcount : Var) -> ST m () [store ::: Store {m} LoggedOut, failcount ::: State Integer]
getData store failcount = do 
  LoginOK <- login store
    | BadPassword => do update failcount (+1)
                        putStrLn ("Wrong password. Failure count: " ++ show !(read failcount))
                        getData store failcount
  scr <- readSecret store
  putStrLn ("secret is: " ++ scr)
  logout store
  getData store failcount


main : IO ()
main = run (do fc <- new 0
               store <- connect
               getData store fc
               disconnect store
               delete fc)

-- Variables:
-- idris-load-packages: ("contrib")
-- End:
 
