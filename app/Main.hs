{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

import           Data.Time               (Day(..))
import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson              hiding (json)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, pack)

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get)
import qualified Database.Persist        as P
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH
import qualified Network.HTTP.Request    as Req
import qualified Data.ByteString.Char8   as B
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.List               as L
import qualified Data.Text               as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  authorId Int
  name Text
  deriving Show
Article json
  articleId Int
  authorId Int
  title Text
  rating Int
  createdAt Day
  deriving Show
|]

data AuthorWithArticles = AuthorWithArticles
  { _authorWithArticlesId :: Int,
    _authorWithArticlesName :: Text,
    articles :: [Article]
  } deriving Show

instance ToJSON AuthorWithArticles where
  toJSON (AuthorWithArticles _authorWithArticlesId _authorWithArticlesName articles) =
    object ["id" .= _authorWithArticlesId,
            "name" .= _authorWithArticlesName,
            "articles" .= articles]

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8181 (spock spockCfg app)

app :: Api
app = do
  get "authors" $ do
    allAuthors <- runSQL $ selectList [] [Asc AuthorId]
    json allAuthors

  post "authors" $ do
    maybeAuthor <- jsonBody' :: ApiAction (Maybe Author)
    case maybeAuthor of
      Nothing -> errorJson 1 "Failed to parse request body as Author"
      Just theAuthor -> do
        newId <- runSQL $ insert theAuthor
        json $ object ["result" .= String "success", "id" .= newId]

  get "articles" $ do
    allArticles <- runSQL $ selectList [] [Asc ArticleId]
    json allArticles

  post "articles" $ do
    maybeArticle <- jsonBody' :: ApiAction (Maybe Article)
    case maybeArticle of
      Nothing -> errorJson 1 "Failed to parse request body as Article"
      Just theArticle -> do
        newId <- runSQL $ insert theArticle
        json $ object ["result" .= String "success", "id" .= newId]

  get "authorswitharticles" $ do
    allAuthors <- runSQL $ selectList [] [Asc AuthorId]
    let authors = fmap entityVal allAuthors
    allArticles <- runSQL $ selectList [] [Asc ArticleId]
    let articles = fmap entityVal allArticles
    let author_with_articles = createAuthorsWithArticles authors articles []    
    json author_with_articles
  
  get ("authorswitharticles" <//> var) $ \filterQuery -> do
    let filters = getFilters filterQuery
    allAuthors <- runSQL $ selectList [] [Asc AuthorId]
    let authors = fmap entityVal allAuthors
    allArticles <- runSQL $ selectList [] [Asc ArticleId]
    let articles = fmap entityVal allArticles
    let author_with_articles = createAuthorsWithArticles authors articles filters
    json author_with_articles

getFilters :: Text -> [Maybe Text]
getFilters query = [T.stripPrefix "filter=" x | x <- T.splitOn "&" query]

getArticlesByAuthorID :: [Article] -> Int -> [Article]
getArticlesByAuthorID articles authorId = [x | x <- articles, articleAuthorId x == authorId]

sortByCreationDate :: [Article] -> [Article]
sortByCreationDate = L.sortOn articleCreatedAt

filterArticles :: [Article] -> [Maybe Text] -> [Article]
filterArticles articles filters
  | Just "ratingGreater3" `elem` filters = filterArticles [x | x <- articles, articleRating x > 3] (L.delete (Just "ratingGreater3") filters)
  | Just "first5" `elem` filters         = filterArticles (take 5 $ sortByCreationDate articles) (L.delete (Just "first5") filters)
  | otherwise                            = articles

createAuthorWithArticles :: Author -> [Article] -> [Maybe Text] -> AuthorWithArticles
createAuthorWithArticles author articles filters = AuthorWithArticles (authorAuthorId author) (authorName author) (filterArticles (getArticlesByAuthorID articles (authorAuthorId author)) filters)

createAuthorsWithArticles :: [Author] -> [Article] -> [Maybe Text] -> [AuthorWithArticles]
createAuthorsWithArticles authors articles filters = [createAuthorWithArticles author articles filters | author <- authors]

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
  object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]
