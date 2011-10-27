import System.Environment ( getArgs )
import System.IO ( Handle, hPutStr, hClose, openTempFile, 
                   hFlush, hSetEncoding, stdout, utf8 )
import Control.Exception ( finally )
import System.FilePath ()
import System.Directory ( getTemporaryDirectory, removeFile )

import Text.CSL
import Text.CSL.Eval
import Text.CSL.Style

import Data.List.Split
import Data.Char
import Text.Printf

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

    let sorts = processSort style refs
    hSetEncoding stdout utf8
    putStr . unlines . (map showKey) $ sorts

showKey :: [Sorting] -> String
showKey ss = 
    case ss of
        [Ascending authors, Ascending year] -> printf "%s%-15s" (tidyAuthors authors) year
        _                                   -> show ss
    where 
      tidyAuthors authors = (concat $ eight authors) :: String
      eight authors = map (printf "%-30s") $ take 6 $ (tidyauthors authors) ++ (repeat "")
      tidyauthors authors = map (map toUpper . dropWhile isSpace . filter strip) $  splitOn ", " authors
      strip c =  (isAlpha c) || (isSpace c)

processSort :: Style -> [Reference] -> [[Sorting]]
processSort (Style {biblio = mb, csMacros = ms , styleLocale = l, csOptions = opts}) rs
    = maybe [] process mb
    where
      opts'   b = mergeOptions (bibOptions b) opts
      sort_   b = evalSorting (EvalSorting emptyCite {citePosition = "first"})l ms (opts' b) (bibSort b)
      process b = map (sort_ b) $ rs

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

