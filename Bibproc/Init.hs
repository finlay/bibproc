module Bibproc.Init( styleFile, myinit ) where

import System.Environment ( getArgs )

import Text.CSL

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
