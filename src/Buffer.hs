module Buffer
    ( createBufferState
    , destroyBufferState
    , createNeededBuffersLoop
    , bufferStateStatus
    , idleCreateBuffersLoop
    , BufferState
    , runProgram
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, retry, writeTVar)
import Control.Exception (bracket)
import Control.Monad (when)
import qualified Data.Map as M

import IdleTime
import Launcher (BufferedProg, destroy, display, launch)
import Util (whisper)

data BufferState = BufferState
    { ready :: TVar (M.Map String (Int, [BufferedProg]))
    , needed :: TVar [String]
    , counter :: TVar Int
    }

createBufferState :: IO BufferState
createBufferState = do
    r <- atomically $ newTVar M.empty
    n <- atomically $ newTVar []
    c <- atomically $ newTVar 0
    return BufferState
        { ready = r
        , needed = n
        , counter = c
        }

destroyBufferState :: BufferState -> IO ()
destroyBufferState buffers = do
    bufs <- atomically $ do
        writeTVar (needed buffers) []
        m <- readTVar (ready buffers)
        writeTVar (ready buffers) M.empty
        return $ concat $ map snd (M.elems m)
    mapM_ destroy bufs

bufferStateStatus :: BufferState -> IO String
bufferStateStatus buffers = do
    bufs <- atomically $ readTVar (ready buffers)
    -- TODO nicer formatting
    return (show bufs)

defaultBufferSize :: Int
defaultBufferSize = 2

createNeededBuffersLoop :: BufferState -> IO ()
createNeededBuffersLoop buffers = do
    hd <- atomically $ do
        queue <- readTVar (needed buffers)
        case queue of
            [] -> retry
            x:xs -> do
                writeTVar (needed buffers) xs
                return x
    createBuffer buffers hd
    createNeededBuffersLoop buffers

defaultPollTime, defaultIdleTime :: Int
defaultPollTime = 2000
defaultIdleTime = 10000

idleCreateBuffersLoop :: BufferState -> IO ()
idleCreateBuffersLoop buffers = do
    let loop :: IdleTimeQuerier -> IO ()
        loop idleQ = do
            idle <- idleTimeQuery idleQ
            whisper $ "idle time: " ++ show idle
            when (idle >= toInteger defaultIdleTime) spawn
            threadDelay (defaultPollTime * 1000)
            loop idleQ
        spawn :: IO ()
        spawn = do
            atomically $ do
                m <- readTVar (ready buffers)
                let mb = M.foldrWithKey f Nothing m
                case mb of
                    Just p -> do
                        queue <- readTVar (needed buffers)
                        writeTVar (needed buffers) (queue ++ [p])
                    Nothing -> return ()
    bracket createIdleTimeQuerier destroyIdleTimeQuerier loop
    where
    f :: String -> (Int, [a]) -> Maybe String -> Maybe String
    f _ _ p@(Just _) = p
    f prog (size, bufs) Nothing = if length bufs < size then Just prog else Nothing

createBuffer :: BufferState -> String -> IO ()
createBuffer buffers prog = do
    whisper $ "creating buffer: " ++ prog
    cid <- atomically $ do
        c <- readTVar (counter buffers)
        writeTVar (counter buffers) (c + 1)
        return c
    buf <- launch prog cid
    atomically $ do
        m <- readTVar (ready buffers)
        case M.lookup prog m of
            Nothing ->
                writeTVar (ready buffers) $ M.insert prog (defaultBufferSize, [buf]) m
            Just (size, bufs) ->
                writeTVar (ready buffers) $ M.insert prog (size, buf:bufs) m

runProgram :: BufferState -> String -> String -> [String] -> IO ()
runProgram buffers prog progPwd progArgs = do
    mBuf <- atomically $ do
        m <- readTVar (ready buffers)
        case M.lookup prog m of
            Just (size, buf:bufs) -> do
                writeTVar (ready buffers) $ M.insert prog (size, bufs) m
                return (Just buf)
            _ -> do
                old <- readTVar (needed buffers)
                writeTVar (needed buffers) (prog:old)
                return Nothing
    case mBuf of
        Just buf -> display buf prog progPwd progArgs
        Nothing -> do
            buf <- atomically $ do
                m <- readTVar (ready buffers)
                case M.lookup prog m of
                    Just (size, buf:bufs) -> do
                        writeTVar (ready buffers) $ M.insert prog (size, bufs) m
                        return buf
                    _ -> retry
            display buf prog progPwd progArgs
