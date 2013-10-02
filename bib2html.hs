import System.Environment ( getArgs )
import System.IO ( hSetEncoding, stdout, utf8 )

import Text.CSL
import Text.Pandoc

main :: IO ()
main = do
    args <- getArgs
    let citefile  :: FilePath
        citefile = 
          case args of
            [filename] -> filename
            _          -> error "Expects one .csl file"
        
    style        <- readCSLFile citefile

    -- Get references from standard in
    bibs <- getContents
    refs <- readBiblioString Bibtex bibs

    let results = processBibliography procOpts style refs
    let wrapPandoc :: Block -> Pandoc
        wrapPandoc blk = Pandoc (Meta [] [] []) [blk]
    let toHtml :: Block -> String
        toHtml = writeHtmlString def . wrapPandoc
    hSetEncoding stdout utf8
    putStr . unlines . map (toHtml . (renderPandoc' style)) $ results

