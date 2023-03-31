import Data.Char
import Data.List qualified as List
import Data.Time.Calendar.Month
import Data.Time.Format
import Hakyll
import Text.HTML.TagSoup
import Text.Hyphenation
import Data.Ord

main :: IO ()
main = hakyllWith defaultConfiguration { destinationDirectory = "docs" } $ do
  -- Load templates
  match "templates/*" $
    compile templateBodyCompiler

  -- Load stylesheets and resources
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
    
  match "resources/*" $ do
    route idRoute
    compile copyFileCompiler

  -- Compile posts
  match "posts/*" $ do
    route $ setExtension "html"

    compile $
      pandocCompiler
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
  let go :: Tag String -> Tag String
      go (TagText text) =
        TagText (hyphenateText text)
      go nonTextTag = nonTextTag
   in itemSetBody (withTagList (fmap go) $ itemBody item) item
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
