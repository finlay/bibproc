module Bibproc.Init( styleFile, myinit, wrapPandoc, toHtml, bibHtml ) where

import System.Environment ( getArgs )

import Text.CSL
import Text.Pandoc


styleFile :: [String] -> FilePath
styleFile args = case args of
            [filename] -> filename
            _          -> error "Expects one .csl file"
        
myinit :: IO (Style, [Reference])
myinit = do
    args <- getArgs
    style <- readCSLFile (styleFile args)
    bibs <- getContents
    refs <- readBiblioString Bibtex bibs

    -- Get references from standard in
    return (style, refs)

wrapPandoc :: Block -> Pandoc
wrapPandoc blk = Pandoc (Meta [] [] []) [blk]

toHtml :: Block -> String
toHtml = writeHtmlString def . wrapPandoc

bibHtml :: Style -> [Reference] -> [String]
bibHtml style refs = map (toHtml . (renderPandoc' style)) results
    where
      results = processBibliography procOpts style refs
