{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module CommandArgs
    ( CommandArgs(..)
    , loadCommandArgs
    )
where

import System.Console.CmdArgs.Implicit
import System.Environment (getProgName)

#ifdef CABAL
import Data.Version (showVersion)
import Paths_instantrun (version)
#endif

programVersion :: String
programVersion =
#ifdef CABAL
    "version " ++ showVersion version
#else
    "unknown-version (not built with cabal)"
#endif

data CommandArgs = CommandArgs
    { config :: Maybe String
    , socket :: Maybe String
    , server :: Bool
    , noDaemon :: Bool
    , status :: Bool
    , shutdown :: Bool
    , flushBuffer :: Bool
    , fillBuffer :: Bool
    , prog :: String
    , files :: [String]
    }
    deriving (Show, Data, Typeable)

dummyCommandArgs :: CommandArgs
dummyCommandArgs = CommandArgs
    { config = Nothing
    , socket = Nothing
    , server = False
    , noDaemon = False
    , status = False
    , shutdown = False
    , flushBuffer = False
    , fillBuffer = False
    , prog = ""
    , files = []
    }

commandArgs :: String -> Annotate Ann
commandArgs progName = record dummyCommandArgs
    [ config      := def += typFile += help "config file"
    , socket      := def += typFile += help "socket file to use"
    , server      := def            += help "start server"
    , noDaemon    := def            += help "do not daemonize (only if --server)"
    , status      := def            += help "show status of server"
    , shutdown    := def            += help "shutdown the server"
    , flushBuffer := def            += help "exit all buffered background programs"
    , fillBuffer  := def            += help "start launching background programs"
    , prog        := def += typ "PROGRAM"      += argPos 0 += opt ""
    , files       := def += typ "PROGRAM-ARGS" += args
    ]
        += verbosity
        += helpArg [name "h", groupname "Help"]
        += versionArg [groupname "Help"]
        += program progName
        += summary (progName ++ ": " ++ programVersion)

loadCommandArgs :: IO CommandArgs
loadCommandArgs = do
    progName <- getProgName
    (cmdArgs_ (commandArgs progName) :: IO CommandArgs)
