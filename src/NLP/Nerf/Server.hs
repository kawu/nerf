module NLP.Nerf.Server
(
-- * Server
  runNerfServer

-- * Client
, ner
, nerX
) where


import           Control.Applicative ((<$), (<$>))
import           Control.Monad (forever, void)
import           Control.Concurrent (forkIO)
import qualified Control.Monad.State.Lazy as S
import           System.IO (Handle, hFlush)
import qualified Data.Text as T
import qualified Data.Traversable as R
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import qualified Network as N

import           Data.Named.Tree
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
        case inp of
            Left x -> do
                let out = Nerf.ner nerf x
                -- putStr "No. of sentences: " >> print (length out)
                sendMsg handle out
            Right x -> do
                let out = Nerf.nerW nerf x
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
    sendTask handle $ Left inp
    recvText handle


-- | Perform NER on a morphosyntactically disambiguated sentence.
-- No re-tokenizetion is performed.
nerX :: N.HostName -> N.PortID -> (w -> Word) -> [w] -> IO (NeForest NE w)
nerX host port f ws = do
    handle <- N.connectTo host port
    sendTask handle $ Right $ map f ws
    replaceLeaves ws <$> recvWord handle


-- | Send message narrowed to the specific data type.
sendTask :: Handle -> Either String [Word] -> IO ()
sendTask = sendMsg


-- | Receive message narrowed to the specific data type.
recvText :: Handle -> IO (NeForest NE T.Text)
recvText = recvMsg


-- | Receive message narrowed to the specific data type.
recvWord :: Handle -> IO (NeForest NE Word)
recvWord = recvMsg



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


-------------------------------------------------
-- Replace leaves
-------------------------------------------------


-- | Replace leaves in the forest by the given list of
-- new elements.  We assume that there is enough elements
-- on the input list.
replaceLeaves :: [w] -> NeForest a b -> NeForest a w
replaceLeaves ws =
    flip S.evalState ws . mapM (R.mapM replace)
    -- flip S.evalState ws . replF
  where
    replace (Left x)  = return $ Left x
    replace (Right _) = Right <$> pop
    pop = do
        xs <- S.get
        case xs of
            (x:xs') -> x <$ S.put xs'
            -- The list of words shorter than the list of leaves:
            []      -> error "Nerf.Server.replaceLeaves: empty list"
--     replT (Node (Left x) ts) = Node (Left x) <$> replF ts
--     replT (Node (Right _) _) = leaf <$> pop
--     replF = mapM replT
--     leaf y = Node (Right y) []
