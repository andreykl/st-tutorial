module Sockets2

import Network.Socket
import Control.ST
import Control.ST.ImplicitCall

data SocketState = Ready | Bound | Open | Closed | Listening

data SocketToClose : SocketState -> Type where
  OpenSocket : SocketToClose Open
  ListeningSocket : SocketToClose Listening

interface Sockets (m : Type -> Type) where
  Sock : SocketState -> Type
  socket : (ty : SocketType)       ->
           ST m (Either () Var) [addIfRight (Sock Ready)]
  close : (sock : Var)                  -> 
          {auto prf : SocketToClose st} -> 
          ST m () [sock ::: (Sock st) :-> Sock Closed]
  remove : (sock : Var) ->
           ST m () [Remove sock (Sock Closed)]
  bind : (sock : Var)                 -> 
         (addr : Maybe SocketAddress) -> 
         (port : Port)                ->
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


-- socket : (fam : SocketFamily) -> (ty : SocketType) -> (pnum : ProtocolNumber) -> IO (Either SocketError Socket)
-- bind : (sock : Socket) -> (addr : Maybe SocketAddress) -> (port : Port) -> IO Int
-- connect : (sock : Socket) -> (addr : SocketAddress) -> (port : Port) -> IO ResultCode
-- listen : (sock : Socket) -> IO Int
-- accept : (sock : Socket) -> IO (Either SocketError (Socket, SocketAddress))
-- send : (sock : Socket) -> (msg  : String) -> IO (Either SocketError ResultCode)
-- recv : (sock : Socket) -> (len : ByteLength) -> IO (Either SocketError (String, ResultCode))
-- close : Socket -> IO ()

Sockets IO where
  Sock _ = State Socket
  socket ty = do
    Right so <- lift $ Socket.socket AF_INET ty 0
      | Left err => pure (Left ())
    sock <- new so
    pure (Right sock)
  close sock = do
    so <- read sock
    lift $ Socket.close so
  remove sock = delete sock
  bind sock addr port = do
    c <- lift $ bind !(read sock) addr port
    if c /= 0
    then pure (Left ())
    else pure (Right ())
  listen sock = do
    c <- lift $ listen !(read sock)
    if c /= 0
    then pure (Left ())
    else pure (Right ())
  accept sock = do
    Right (newsock, _) <- lift $ accept !(read sock)
      | Left err => pure (Left ())
    ns <- new newsock
    returning (Right ns) (toEnd ns)
  send sock msg = do
    Right _ <- lift $ send !(read sock) msg
      | Left _ => pure (Left ())
    pure (Right ())
  recv sock = do
    Right (msg, _) <- lift $ recv !(read sock) 1024
      | Left _ => pure (Left ())
    pure (Right msg)

echoServer : (ConsoleIO m, Sockets m) => (sock : Var) -> ST m () [sock ::: Sock {m} Listening :-> Sock {m} Closed]
echoServer sock = do
  Right ns <- accept sock     | Left err => close sock
  Right msg <- recv ns        | Left err => do remove ns; close sock
  Right ok <- send ns msg     | Left err => do remove ns; close sock
  close ns; remove ns
  echoServer sock

startServer : (ConsoleIO m, Sockets m) => ST m () []
startServer = do
  Right sock <- socket Stream             | Left err => putStrLn "error creating sock"
  Right ok <- bind sock Nothing 9442      | Left err => remove sock
  Right ok <- listen sock                 | Left err => remove sock
  echoServer sock
  remove sock

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
 
