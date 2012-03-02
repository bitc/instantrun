module Server
    ( startServer
    ) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, tryJust)
import Control.Monad (guard)
import Control.Monad (when)
import Network (listenOn, PortID(UnixSocket), Socket, accept, sClose)
import System.Directory (removeFile)
import System.IO (Handle, hGetLine, hPutStrLn, hClose)
import System.IO.Error (isDoesNotExistError)

import Buffer
import Config (Config, socketPath)
import Util (whisper, unescapeStr)

startServer :: Config -> IO ()
startServer cfg = do
    let removeSocketFile :: IO ()
        removeSocketFile = do
            whisper $ "Deleting socket file " ++ socketPath cfg
            -- Ignore possible error if socket file does not exist
            _ <- tryJust (guard . isDoesNotExistError) $ removeFile (socketPath cfg)
            return ()
    whisper $ "Starting server on socket " ++ socketPath cfg
    removeSocketFile
    bracket
        (listenOn (UnixSocket (socketPath cfg)))
        (\sock -> sClose sock >> removeSocketFile)
        $ \sock -> do
            bracket createBufferState destroyBufferState $ \bufferState -> do
                -- TODO kill these threads during shutdown
                _ <- forkIO (createNeededBuffersLoop bufferState)
                _ <- forkIO (idleCreateBuffersLoop bufferState)
                waitForConnection bufferState sock

runCommand :: BufferState -> String -> FilePath -> [String] -> IO ()
runCommand buffers progName progPwd progArgs = do
    whisper $ "Running command " ++ progName ++ " " ++ progPwd ++ " " ++ show progArgs
    runProgram buffers progName progPwd progArgs

waitForConnection :: BufferState -> Socket -> IO ()
waitForConnection buffers sock = do
    -- TODO handle IOException for accept
    (h, _, _) <- accept sock
    continue <- handleConnection buffers h
    hClose h
    when continue $ waitForConnection buffers sock

handleConnection :: BufferState -> Handle -> IO Bool
handleConnection buffers h = do
    whisper $ "Client connected"
    cmd <- hGetLine h
    case cmd of
        "run" -> do
            progName <- hGetLine h
            progPwd <- hGetLine h
            progArgs <- readArgs []
            runCommand buffers (unescapeStr progName) (unescapeStr progPwd) (reverse progArgs)
            hPutStrLn h "Ok"
            return True
        "status" -> do
            whisper $ "Sending status"
            status <- bufferStateStatus buffers
            hPutStrLn h status
            return True
        "shutdown" -> do
            whisper $ "Shutting down"
            hPutStrLn h "Ok"
            return False
        "flushbuffer" -> error "TODO"
        "fillbuffer" -> error "TODO"
        _ -> do
            whisper $ "Invalid command: " ++ cmd
            hPutStrLn h $ "Invalid command: " ++ cmd
            return True

    where
    readArgs :: [String] -> IO [String]
    readArgs xs = do
        t <- hGetLine h
        case null t of
            True -> return xs
            False -> readArgs ((unescapeStr t):xs)
