module Test.Framework.Runners.Console.Utilities (
        hideCursorDuring
    ) where

import System.Console.ANSI ( hideCursor, showCursor )
import System.IO ( hFlush, stdout )

import Control.Exception (bracket)


hideCursorDuring :: IO a -> IO a
hideCursorDuring action = bracket hideCursor (const (showCursor >> hFlush stdout)) (const action)
