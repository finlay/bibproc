import System.Environment ( getArgs )
import System.IO ( Handle, hPutStr, hClose, openTempFile, 
                   hFlush, hSetEncoding, stdout, utf8 )
import Control.Exception ( finally )
import System.FilePath ()
import System.Directory ( getTemporaryDirectory, removeFile )

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
    refs <- withTempFile "bibfile.bib" getReferences

    let results = processBibliography procOpts style refs
    let wrapPandoc :: Block -> Pandoc
        wrapPandoc blk = Pandoc (Meta [] [] []) [blk]
    let toHtml :: Block -> String
        toHtml = writeHtmlString defaultWriterOptions . wrapPandoc
    hSetEncoding stdout utf8
    putStr . unlines . map (toHtml . (renderPandoc' style)) $ results

getReferences :: FilePath -> Handle -> IO [Reference]
getReferences bibfile bibhandle = do
    bibs <- getContents 
    hSetEncoding bibhandle utf8
    hPutStr bibhandle bibs
    hFlush bibhandle
    readBiblioFile bibfile

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
       (tempfile, temph) <- openTempFile tempdir pattern 

       finally (func tempfile temph) 
               (hClose temph >> removeFile tempfile)
