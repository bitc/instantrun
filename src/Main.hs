module Main where

import Control.Exception (tryJust)
import Control.Monad (guard)
import Network (connectTo, PortID(UnixSocket))
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Error (isDoesNotExistError)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.IO
import System.Posix.Process (exitImmediately, createSession, forkProcess)
import Control.Concurrent (forkIO, threadDelay)

import Client (runProgram, serverShutdown, serverStatus)
import Config
import Server (startServer)
import Util (whisper)

main :: IO ()
main = do
    cfg <- loadConfig
    let connect = do
            whisper $ "Connecting to " ++ socketPath cfg
            connectTo "" (UnixSocket (socketPath cfg))
    case commandMode cfg of
        CommandServer False -> daemonize $ startServer cfg
        CommandServer True -> startServer cfg
        CommandStatus -> connect >>= serverStatus
        CommandShutdown -> connect >>= serverShutdown
        CommandFlushBuffer -> error "TODO"
        CommandFillBuffer -> error "TODO"
        CommandRun prog args -> if (null prog)
            then
                -- TODO clean up
                putStrLn "You must supply an argument"
            else do
                r <- tryJust (guard . isDoesNotExistError) connect
                case r of
                    Left e -> do
                        whisper $ "Connection failed: " ++ (show e)
                        daemonize $ do
                            _ <- forkIO $ do
                                threadDelay 1000000
                                putStrLn "whoah"
                            startServer cfg
                    Right handle -> do
                        pwd <- getCurrentDirectory
                        runProgram handle prog pwd args

daemonize :: IO () -> IO ()
daemonize program = do
    _ <- forkProcess child1
    exitImmediately ExitSuccess

    where
    child1 = do
        _ <- createSession
        _ <- forkProcess child2
        exitImmediately ExitSuccess

    child2 = do
        changeWorkingDirectory "/"
        mapM_ closeFd [stdInput, stdOutput, stdError]
        nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
        mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
        closeFd nullFd
        program
