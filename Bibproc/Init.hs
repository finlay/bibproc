module Bibproc.Init(
    myinit, styleFile, 
    wrapPandoc, toHtml, bibHtml,
    processSort, showKey
    ) where

import System.Environment ( getArgs )

import Text.CSL
import Text.CSL.Eval
import Text.CSL.Style
import Text.Pandoc

import Data.List.Split
import Data.Char
import Text.Printf


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
processSort (Style {biblio = mb, csMacros = ms , styleLocale = l, styleAbbrevs = sa, csOptions = opts}) rs
    = maybe [] process mb
    where
      opts'   b = mergeOptions (bibOptions b) opts
      sort_   b = evalSorting (EvalSorting emptyCite {citePosition = "first"}) l ms (opts' b) (bibSort b) sa
      process b = map (sort_ b) $ rs
