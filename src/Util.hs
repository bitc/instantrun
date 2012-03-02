module Util
    ( whisper
    , escapeStr
    , unescapeStr
    , prop_unescape_escape
    , prop_escape_notnull
    , prop_escape_nonewlines
    , prop_unescape_total
    ) where

import System.Console.CmdArgs.Verbosity (whenLoud)
import System.IO (hPutStrLn, stderr)

whisper :: String -> IO ()
whisper msg = whenLoud $ hPutStrLn stderr msg

escapeStr :: String -> String
escapeStr = show

unescapeStr :: String -> String
unescapeStr str =
    case reads str of
        [(a, "")] -> a
        _ -> str

prop_unescape_escape :: String -> Bool
prop_unescape_escape str = str == (unescapeStr . escapeStr) str

prop_escape_notnull  :: String -> Bool
prop_escape_notnull str = (not . null) (escapeStr str)

prop_escape_nonewlines :: String -> Bool
prop_escape_nonewlines str = notElem '\n' (escapeStr str)

prop_unescape_total :: String -> Bool
prop_unescape_total str = length (unescapeStr str) >= 0 -- Force evaluation
