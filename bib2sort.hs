import System.IO ( hSetEncoding, stdout, utf8 )

import Bibproc.Init (myinit, processSort, showKey)

main :: IO ()
main = do
    (style, refs) <- myinit

    let sorts = processSort style refs
    hSetEncoding stdout utf8
    putStr . unlines . (map showKey) $ sorts

