module Sockets

import Control.ST
import Control.ST.ImplicitCall
import Network.Socket

data SocketState = Ready | Bound | Listening | Open | Closed

data CloseOK : SocketState -> Type where
  CloseOpen : CloseOK Open
  CloseListening : CloseOK Listening

interface Sockets (m : Type -> Type) where
  Sock : SocketState -> Type
  socket : SocketType -> ST m (Either () Var) [addIfRight (Sock Ready)]
  bind : (sock : Var) -> (addr : Maybe SocketAddress) -> (port : Port) ->
         ST m (Either () ()) 
           [sock ::: Sock Ready :-> (Sock Closed `or` Sock Bound)]
  listen : (sock : Var) ->
           ST m (Either () ()) 
             [sock ::: Sock Bound :-> (Sock Closed `or` Sock Listening)]
  accept : (sock : Var) ->
           ST m (Either () Var)
             [sock ::: Sock Listening, addIfRight (Sock Open)]
  send : (sock : Var) -> String ->
         ST m (Either () ()) 
           [sock ::: Sock Open :-> (Sock Closed `or` Sock Open)]
  recv : (sock : Var) ->
         ST m (Either () String)
           [sock ::: Sock Open :-> (Sock Closed `or` Sock Open)]
  close : (sock : Var) ->
          {auto prf : CloseOK st} ->
          ST m () [sock ::: Sock st :-> Sock Closed]
  remove : (sock : Var) -> ST m () [Remove sock (Sock Closed)]
  
  connect : (sock : Var) -> SocketAddress -> Port ->
            ST m (Either () ()) 
              [sock ::: Sock Ready :-> (Sock Closed `or` Sock Open)]

Sockets IO where
  Sock _ = State Socket
  socket ty = do
    Right sock <- lift $ Socket.socket AF_INET ty 0
      | Left err => pure (Left ())
    lbl <- new sock
    pure (Right lbl)
  bind sock addr port = do
    so <- read sock
    v <- lift $ Socket.bind so addr port
    if v /= 0
       then pure (Left ())
       else pure (Right ())
  listen sock = do
    err <- lift $ Socket.listen !(read sock)
    if err /= 0
      then pure (Left ())
      else pure (Right ())
  accept sock = do
    Right (newsock, _) <- lift $ Socket.accept !(read sock)
      | Left err => pure (Left ())
    ns <- new newsock
    returning (Right ns) (toEnd ns)
  send sock msg = do
    Right code <- lift $ Socket.send !(read sock) msg
      | Left _ => pure (Left ())
    pure (Right ())
  recv sock = do
    Right (msg, _) <- lift $ Socket.recv !(read sock) 1024
      | Left err => pure (Left ())
    pure (Right msg)
  close sock = lift $ Socket.close !(read sock)
  remove sock = delete sock
  connect sock addr port = do
    c <- lift $ Socket.connect !(read sock) addr port
    if c /= 0
    then pure (Left ())
    else pure (Right ())


echoServer : (ConsoleIO m, Sockets m) => (sock : Var) ->
             ST m () [remove sock (Sock {m} Listening)]
echoServer sock =
  do Right ns <- accept sock | Left err => do close sock; remove sock
     Right msg <- recv ns | Left err => do close sock; remove sock; remove ns
     Right ok <- send ns ("You said " ++ msg)
           | Left err => do remove ns; close sock; remove sock
     close ns; remove ns
     echoServer sock

startServer : (ConsoleIO m, Sockets m) => ST m () []
startServer = do
  Right sock <- socket Stream        | Left err => pure ()
  Right ok <- bind sock Nothing 9442 | Left err => remove sock
  Right ok <- listen sock            | Left err => remove sock
  echoServer sock

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
