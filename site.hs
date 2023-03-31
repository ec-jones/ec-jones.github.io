import Data.Char
import Data.List qualified as List
import Data.Ord
import Data.Time.Calendar.Month
import Data.Time.Format
import Hakyll
import Text.HTML.TagSoup
import Text.Hyphenation
import Text.Pandoc

main :: IO ()
main = hakyllWith defaultConfiguration {destinationDirectory = "docs"} $ do
  -- Load templates
  match "templates/*" $
    compile templateBodyCompiler

  -- Load stylesheets
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  -- Load resources
  match
    ( ("resources/*" .&&. complement "resources/cv/*")
        .||. "resources/cv/Eddie Jones - CV.pdf"
    )
    $ do
      route (gsubRoute "resources/cv/" (const "resources/"))
      compile copyFileCompiler

  -- Compile posts
  match "posts/*" $ do
    route $ setExtension "html"

    compile $
      pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions { 
            writerHTMLMathMethod = MathJax ""
              }
        >>= ( loadAndApplyTemplate "templates/default.html" defaultContext
                . hyphenateHtml
            )
        >>= relativizeUrls

  -- Compile index
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*"

      let indexContext :: Context String
          indexContext =
            listField "posts" defaultContext (sortPosts posts)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext
        >>= relativizeUrls

-- Sort posts by date.
sortPosts :: [Item String] -> Compiler [Item String]
sortPosts =
  let go :: Item String -> Compiler (Item String, Down Month)
      go post = do
        date <- getMetadataField' (itemIdentifier post) "date"
        date' <- parseTimeM True defaultTimeLocale "%B %Y" date
        pure (post, Down date')
   in fmap (fmap fst . List.sortOn snd) . mapM go

-- Hyphenate paragraphs
hyphenateHtml :: Item String -> Item String
hyphenateHtml item =
  let go :: Mode -> Tag String -> (Mode, Tag String)
      go _ (TagOpen "span" attr)
        | Just cls <- lookup "class" attr,
          "math" `List.isPrefixOf` cls =
            -- Enter maths mode, i.e. don't hyphenate.
            (MathMode, TagOpen "span" attr)
      go _ (TagClose "span") =
        -- Exit maths mode, i.e. return to text mode.
        -- Maths is assumed to not contain any spans.
        (TextMode, TagClose "span")
      go TextMode (TagText text) =
         -- Only hyphenate in text mode.
        (TextMode, TagText (hyphenateText text))
      go mode nonTextTag = (mode, nonTextTag)
   in itemSetBody (withTagList (snd . List.mapAccumL go TextMode) $ itemBody item) item
  where
    hyphenateText :: String -> String
    hyphenateText str
      | null str = ""
      | otherwise =
          let (whiteSpace, str') = span isSpace str
              (word, str'') = break isSpace str'
           in whiteSpace
                ++ List.intercalate "\x00ad" (hyphenate english_GB word)
                ++ hyphenateText str''

data Mode
  = TextMode
  | MathMode