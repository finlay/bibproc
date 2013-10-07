import System.IO ( hSetEncoding, stdout, utf8 )

import Bibproc.Init

main :: IO ()
main = do
    (style, refs) <- myinit

    hSetEncoding stdout utf8
    putStr . unlines $ bibHtml style refs
