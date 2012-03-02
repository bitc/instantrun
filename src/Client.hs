module Client
    ( runProgram
    , serverStatus
    , serverShutdown
    ) where

import System.IO (Handle, hPutStrLn, hGetLine, hClose, hFlush)

import Util (whisper, escapeStr)

runProgram :: Handle -> String -> String -> [String] -> IO ()
runProgram h progName pwd progArgs = do
    whisper $ "sending command"
    hPutStrLn h "run"
    hPutStrLn h (escapeStr progName)
    hPutStrLn h (escapeStr pwd)
    mapM_ (hPutStrLn h) (map escapeStr progArgs)
    hPutStrLn h ""
    hFlush h
    response <- hGetLine h
    whisper response
    hClose h

serverStatus :: Handle -> IO ()
serverStatus h = do
    whisper $ "sending command"
    hPutStrLn h "status"
    hFlush h
    whisper $ "reading response"
    response <- hGetLine h
    putStrLn response
    hClose h

serverShutdown :: Handle -> IO ()
serverShutdown h = do
    whisper $ "sending command"
    hPutStrLn h "shutdown"
    hFlush h
    whisper $ "reading response"
    response <- hGetLine h
    whisper response
    hClose h
