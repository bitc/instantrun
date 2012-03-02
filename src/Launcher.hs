module Launcher
    ( BufferedProg
    , launch
    , display
    , destroy
    ) where

import Control.Concurrent (threadWaitRead)
import Control.Exception (IOException, try)
import Control.Monad (when)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Directory (getHomeDirectory)
import System.Environment (getEnvironment)
import System.Posix (ProcessID)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Posix.Types (Fd(Fd))
import System.Process (readProcess)

import Config (configGetCmds)
import Util (whisper)

data BufferedProg = BufferedProg Window Int
    deriving (Show)

runCommandChild :: String -> FilePath -> [String] -> [(String, String)] -> IO ProcessID
runCommandChild cmd cwd args childEnv = do
    myEnv <- getEnvironment
    forkProcess $ do
        _ <- createSession
        changeWorkingDirectory cwd
        executeFile "/bin/sh" False (["-c", cmd, "/bin/sh"] ++ args) (Just (myEnv ++ childEnv))

launch :: String -> Int -> IO BufferedProg
launch prog cid = do
    dpy <- openDisplay ""

    let command = case configGetCmds prog of
            Nothing -> prog
            Just (start, _) -> start

    whisper $ "Running start command: " ++ command
    home <- getHomeDirectory
    pid <- runCommandChild command home [] [("INSTANTRUN_ID", show cid)]

    selectInput dpy (defaultRootWindow dpy) substructureNotifyMask
    --xa_wm_state <- internAtom dpy "WM_STATE" False

    let waitEvents = do
            numPending <- pending dpy
            when (numPending == 0) $ do
                threadWaitRead (Fd (connectionNumber dpy))
                waitEvents

    let processEvent p = do
            waitEvents
            nextEvent dpy p
            event <- getEvent p
            case event of
                MapNotifyEvent _ _ _ _ _ window _ -> do
                    atom <- internAtom dpy "_NET_WM_PID" True
                    props <- getWindowProperty32 dpy atom window
                    case props of
                        Just [a] -> do
                            check <- processIsAncestor (fromInteger (toInteger a)) pid
                            if check
                                then do
                                    unmapWindow dpy window
                                    return window
                                else processEvent p
                        _ -> processEvent p
                _ -> processEvent p

    window <- allocaXEvent processEvent

    closeDisplay dpy

    return (BufferedProg window cid)

display :: BufferedProg -> String -> String -> [String] -> IO ()
display (BufferedProg window cid) prog progPwd progArgs = do
    whisper $ "Displaying window"
    dpy <- openDisplay ""
    mapWindow dpy window
    flush dpy
    closeDisplay dpy

    case configGetCmds prog of
        Nothing -> return ()
        Just (_, remote) -> do
            whisper $ "Running remote command: " ++ remote
            _ <- runCommandChild remote progPwd progArgs [("INSTANTRUN_ID", show cid)]
            return ()

destroy :: BufferedProg -> IO ()
destroy (BufferedProg window _) = do
    whisper $ "Destroying window"
    dpy <- openDisplay ""
    _ <- killClient dpy window
    flush dpy
    closeDisplay dpy

processParent :: ProcessID -> IO (Maybe ProcessID)
processParent pid = do
    let n :: Integer
        n = toInteger pid
    if n == 1 then return Nothing
        else do
            -- TODO mute stderr of child process
            r <- try $ readProcess "ps" ["-o", "ppid", "-p", show n] ""
            case r of
                Left e -> return (ignore e)
                Right out -> return $ parse (lines out)
    where
    ignore :: IOException -> Maybe a
    ignore _ = Nothing
    parse :: [String] -> Maybe ProcessID
    parse [] = Nothing
    parse (_:[]) = Nothing
    parse (_:x:_) = tryRead x
    tryRead x = case reads x of
        [(a, "")] -> Just a
        _ -> Nothing

processIsAncestor :: ProcessID -> ProcessID -> IO Bool
processIsAncestor pid ancestor =
    if pid == ancestor then return True
        else do
            parent <- processParent pid
            case parent of
                Nothing -> return False
                Just p -> processIsAncestor p ancestor
