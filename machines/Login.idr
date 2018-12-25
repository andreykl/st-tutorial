module Login

import Control.ST

data Access = LoggedOut | LoggedIn

data LoginResult = OK | BadPassword

interface DataStore (m : Type -> Type) where
  Store : Access -> Type
  connect : ST m Var [add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () [remove store (Store LoggedOut)]
  readSecret : (store : Var) -> ST m String [store ::: Store LoggedIn]
  login : (store : Var) -> 
          ST m LoginResult [store ::: Store LoggedOut :-> 
                   (\res => Store (case res of
                             OK => LoggedIn
                             BadPassword => LoggedOut))]
  logout : (store : Var) ->
           ST m () [store ::: Store LoggedIn :-> Store LoggedOut]

-- interface DataStore (m : Type -> Type) where
--   Store : Access -> Type

--   connect : ST m Var [add (Store LoggedOut)]
--   disconnect : (store : Var) -> ST m () [remove store (Store LoggedOut)]

--   readSecret : (store : Var) -> ST m String [store ::: Store LoggedIn]
--   login : (store : Var) ->
--           ST m LoginResult [store ::: Store LoggedOut :->
--                              (\res => Store (case res of
--                                                   OK => LoggedIn
--                                                   BadPassword => LoggedOut))]
--   logout : (store : Var) -> ST m () [store ::: Store LoggedIn :-> Store LoggedOut]


doConnect : DataStore m => ST m () []
doConnect = do st <- connect
               disconnect st


getData : (ConsoleIO m, DataStore m) => ST m () []
getData = do
  st <- connect
  ok <- login st
  case ok of
    OK => do putStrLn "Logged in succesfully. Reading secret."
             sec <- readSecret st
             putStrLn ("Secret is " ++ sec ++ ". Logging out.")
             logout st
             disconnect st
    BadPassword => do putStrLn "Bad password."
                      disconnect st

DataStore IO where  
  Store x = State String
  connect = do store <- new "Secret Data"
               pure store
  disconnect store = delete store
  readSecret store = do s <- read store
                        pure s
  login store = do
    putStr "Enter password: "
    p <- getStr
    if p == "password"
    then pure OK
    else pure BadPassword
  logout store = pure ()

-- badGet : DataStore m => ST m () []
-- badGet = do st <- connect
--             secret <- readSecret st
--             disconnect st

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
