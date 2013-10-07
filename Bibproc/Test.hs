{-# LANGUAGE QuasiQuotes #-}
-- file bib2html_test.hs
import Bibproc.Init

import Text.CSL
import System.Exit
import Test.HUnit hiding (counts)

import Text.Heredoc

testArticle :: Test
testArticle = TestCase ( do 
    style <- readCSLFile (styleFile ["./test/apa-dragonfly.csl"])
    refs <- readBiblioString Bibtex [here|
@ARTICLE{brothers_seabird_2010,
  author = {Nigel Brothers and Alan R. Duckworth and Carl Safina and Eric L. Gilman},
  title = {Seabird Bycatch in Pelagic Longline Fisheries Is Grossly Underestimated when Using Only Haul Data},
  journal = {{PLoS} {ONE}},
  year = {2010},
  volume = {5},
  pages = {e12491},
  number = {8},
  doi = {10.1371/journal.pone.0012491},
  url = {http://dx.doi.org/10.1371/journal.pone.0012491.},
  abstract = {"The chicken or the egg"},
  lastchecked = {5 November 2010}
}
|]
    assertEqual "Test bibHtml on @Article" ["<p>Brothers, N., Duckworth, A. R., Safina, C., &amp; Gilman, E. L. (2010). Seabird Bycatch in Pelagic Longline Fisheries Is Grossly Underestimated when Using Only Haul Data. <em>PLoS ONE</em>, <em>5</em>(8), e12491. doi:10.1371/journal.pone.0012491</p>"] (bibHtml style refs))

testUnpublished :: Test
testUnpublished = TestCase ( do 
    style <- readCSLFile (styleFile ["./test/apa-dragonfly.csl"])
    refs <- readBiblioString Bibtex [here|
@unpublished{brothers_seabird_2010,
  author = {Nigel Brothers and Alan R. Duckworth and Carl Safina and Eric L. Gilman},
  title = {Seabird Bycatch in Pelagic Longline Fisheries Is Grossly Underestimated when Using Only Haul Data},
  year = {2010},
  doi = {10.1371/journal.pone.0012491},
  url = {http://dx.doi.org/10.1371/journal.pone.0012491.},
}
|]
    assertEqual "Test bibHtml on @Unpublished" ["<p>Brothers, N., Duckworth, A. R., Safina, C., &amp; Gilman, E. L. (2010). Seabird Bycatch in Pelagic Longline Fisheries Is Grossly Underestimated when Using Only Haul Data. doi:10.1371/journal.pone.0012491</p>"] (bibHtml style refs))

flergl :: Char -> Maybe Char
flergl = const Nothing

testEmpty :: Test
testEmpty = TestCase $ assertEqual 
  "Should get Nothing from an empty string" Nothing ( flergl 'x' ) 

tests :: Test
tests = TestList [testArticle, testUnpublished, testEmpty]


main :: IO ()
main = do
    counts <- runTestTT tests
    let issues = (errors counts) + (failures counts)
    exitWith $ if issues == 0 then ExitSuccess else ExitFailure issues
