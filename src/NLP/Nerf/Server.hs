module NLP.Nerf.Server
(
-- * Server
  runNerfServer

-- * Client
, ner
) where


import           Control.Applicative ((<$>))
import           Control.Monad (forever, void)
import           Control.Concurrent (forkIO)
import           System.IO (Handle, hFlush)
import qualified Data.Text as T
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import qualified Network as N

import           Data.Named.Tree (NeForest)
import           NLP.Nerf.Types
import           NLP.Nerf (Nerf)
import qualified NLP.Nerf as Nerf


-------------------------------------------------
-- Server
-------------------------------------------------


-- | Run a Nerf server on a given port.
runNerfServer :: Nerf -> N.PortID -> IO ()
runNerfServer nerf port = N.withSocketsDo $ do
    sock <- N.listenOn port
    forever $ sockHandler nerf sock


sockHandler :: Nerf -> N.Socket -> IO ()
sockHandler nerf sock = do
    (handle, _, _) <- N.accept sock
    -- putStrLn "Connection established"
    void $ forkIO $ do
        -- putStrLn "Waiting for input..."
        inp <- recvMsg handle
        -- putStr "> " >> T.putStrLn inp
        let out = Nerf.ner nerf inp
        -- putStr "No. of sentences: " >> print (length out)
        sendMsg handle out


-------------------------------------------------
-- Client
-------------------------------------------------


-- | Perform NER tagging on the input sentence.
-- ner :: N.HostName -> N.PortID -> String -> IO (NeForest NE Word)
ner :: N.HostName -> N.PortID -> String -> IO (NeForest NE T.Text)
ner host port inp = do
    handle <- N.connectTo host port
    -- putStrLn "Connection established"
    -- putStr "Send request: " >> putStrLn inp
    sendMsg handle inp
    recvMsg handle


-------------------------------------------------
-- Messages
-------------------------------------------------


sendMsg :: B.Binary a => Handle -> a -> IO ()
sendMsg h msg = do
    let x = B.encode msg
        n = fromIntegral $ BS.length x
    sendInt h n
    BS.hPut h x
    hFlush h


recvMsg :: B.Binary a => Handle -> IO a
recvMsg h = do
    n <- recvInt h
    B.decode <$> BS.hGet h n


sendInt :: Handle -> Int -> IO ()
sendInt h x = BS.hPut h (B.encode x)


-- | TODO: is it safe to assume that the length of the
-- `Int` representation is 8?
recvInt :: Handle -> IO Int
recvInt h = B.decode <$> BS.hGet h 8
