{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.HTTP.Request as Req
import qualified Data.ByteString.Lazy as B
import Data.Text
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Time
import System.Directory
import Data.List as L

data Author = Author {
  _authorId :: Int,
  _name :: Text
} deriving Show

instance ToJSON Author where
  toJSON (Author id name) =
    object ["authorId" .= id,
            "name" .= name]

instance FromJSON Author where
  parseJSON = withObject "Author" $ \a -> Author
    <$> a .: "id"
    <*> a .: "name"

data Article = Article {
  _articleArticleId :: Int,
  _articleAuthorId :: Int,
  _articleTitle :: Text,
  _articleRating :: Int,
  _articleCreatedAt :: Day
} deriving Show

instance ToJSON Article where
  toJSON (Article id auth_id title rating date) = 
    object ["articleId" .= id,
            "authorId" .= auth_id,
            "title" .= title,
            "rating" .= rating,
            "createdAt" .= date]

instance FromJSON Article where
  parseJSON = withObject "Article" $ \a -> Article
    <$> a.: "id"
    <*> a.: "author_id"
    <*> a.: "title"
    <*> a.: "rating"
    <*> a.: "created_at"

main :: IO ()
main = do
  Just authors <- getAuthors "app-add-data/authors.json"
  Just articles <- getArticles "app-add-data/articles.json"
  manager <- newManager defaultManagerSettings
  addAuthors manager authors
  addArticles manager articles

getAuthors :: String -> IO (Maybe [Author])
getAuthors filename = do
  fileExists <- doesFileExist filename
  if fileExists
    then do author_contents <- B.readFile filename
            let (Just authors) = decode author_contents :: Maybe [Author]
            return $ Just authors
    else do putStrLn $ "The file " ++ filename ++ " does not exist!"
            return Nothing

getArticles :: String -> IO (Maybe [Article])
getArticles filename = do
  fileExists <- doesFileExist filename
  if fileExists
    then do article_contents <- B.readFile filename
            let (Just articles) = decode article_contents :: Maybe [Article]
            return $ Just articles
    else do putStrLn $ "The file " ++ filename ++ " does not exist!"
            return Nothing

addAuthors :: Manager -> [Author] -> IO()
addAuthors manager (author:authors) = do
  req <- buildPostRequest author "http://localhost:8181/authors"
  res <- httpLbs req manager
  addAuthors manager authors
addAuthors manager [] = do
  print "All authors added"

addArticles :: Manager -> [Article] -> IO()
addArticles manager (article:articles) = do
  req <- buildPostRequest article "http://localhost:8181/articles"
  res <- httpLbs req manager
  addArticles manager articles
addArticles manager [] = do
  print "All articles added"

buildPostRequest :: (ToJSON a) => a -> String -> IO Request
buildPostRequest obj url = do
  initialRequest <- parseRequest url
  let request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode obj }
  pure request
