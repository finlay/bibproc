import System.IO ( hSetEncoding, stdout, utf8 )

import Text.CSL
import Text.Pandoc

import Bibproc.Init

wrapPandoc :: Block -> Pandoc
wrapPandoc blk = Pandoc (Meta [] [] []) [blk]

toHtml :: Block -> String
toHtml = writeHtmlString def . wrapPandoc

bibHtml :: Style -> [Reference] -> [String]
bibHtml style refs = map (toHtml . (renderPandoc' style)) results
    where
      results = processBibliography procOpts style refs

main :: IO ()
main = do
    (style, refs) <- myinit

    hSetEncoding stdout utf8
    putStr . unlines $ bibHtml style refs
