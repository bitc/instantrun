module Config
    ( loadConfig
    , Config(..)
    , configGetCmds
    , CommandMode(..)
    ) where

import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory)
import System.FilePath (combine)

import CommandArgs

defaultSocketFilename, defaultConfigFilename :: FilePath
defaultSocketFilename = "instantrun.sock"
defaultConfigFilename = "instantrun.conf"

defaultFile :: Maybe String -> String -> IO String
defaultFile opt def = do
    case opt of
        Just s -> return s
        Nothing -> do
            appDir <- getAppUserDataDirectory "instantrun"
            createDirectoryIfMissing False appDir
            return $ appDir `combine` def

configure :: CommandArgs -> IO Config
configure args = do
    c <- defaultFile (config args) defaultConfigFilename
    s <- defaultFile (socket args) defaultSocketFilename
    let cmd
            | server args = CommandServer (noDaemon args)
            | status args = CommandStatus
            | shutdown args = CommandShutdown
            | flushBuffer args = CommandFlushBuffer
            | fillBuffer args = CommandFillBuffer
            | otherwise = CommandRun (prog args) (files args)
    return Config
        { configPath = c
        , socketPath = s
        , commandMode = cmd
        }

loadConfig :: IO Config
loadConfig = loadCommandArgs >>= configure

data CommandMode
    = CommandServer Bool
    | CommandRun String [String]
    | CommandStatus
    | CommandShutdown
    | CommandFlushBuffer
    | CommandFillBuffer

data Config = Config
    { configPath :: FilePath
    , socketPath :: FilePath
    , commandMode :: CommandMode
    }


configGetCmds :: String -> Maybe (String, String)
configGetCmds "gvim" =
    Just
        ( "gvim -f --servername IRUN${INSTANTRUN_ID}"
        , "gvim --servername IRUN${INSTANTRUN_ID} --remote-send \":cd $(echo ${PWD} | sed -e 's/ /\\\\ /g')<CR>\";" ++
            "gvim --servername IRUN${INSTANTRUN_ID} --remote \"$@\"")
configGetCmds _ = Nothing
