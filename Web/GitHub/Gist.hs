{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Web.GitHub.Gist
    (
    Gist(..)
    )
where

import Control.Applicative
import Control.Failure
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Conduit

import Web.GitHub.Internal.Request

data Gist = Gist {
    gistUrl :: T.Text,
    gistId :: Integer,
    gistDescription :: Maybe T.Text,
    gistUser :: GistUser,
    gistCommentCount :: Int,
    gistHtmlUrl :: T.Text,
    gistPullUrl :: T.Text,
    gistPushUrl :: T.Text,
    gistCreatedAt :: T.Text
    }
    deriving (Show, Read, Eq)

-- | Represents a User from the result of a Gist query. This doesn't contain
-- all of the same data as a normal User, so to convert another HTTPS query
-- must be made to GitHub to get more detail about the User.
data GistUser = GistUser {
    gistUserLogin :: T.Text,
    gistUserId :: Integer,
    gistUserAvatarUrl :: T.Text,
    gistUserGravatarId :: T.Text,
    gistUserUrl :: T.Text
    }
    deriving (Show, Read, Eq)

instance FromJSON Gist where
    parseJSON (Object o) = Gist
        <$> o .: "url"
        <*> liftM read (o .: "id")
        <*> o .:? "description"
        <*> o .: "user"
        <*> o .: "comments"
        <*> o .: "html_url"
        <*> o .: "git_pull_url"
        <*> o .: "git_push_url"
        <*> o .: "created_at"

    parseJSON _ = mzero

getGist :: (Failure HttpException m, MonadResource m, MonadBaseControl IO m)
        => Integer 
        -> Manager
        -> m Gist
getGist id m = do req <- parseUrl $ "https://api.github.com/gists/" ++ show id
                  (val, headers) <- simpleRequest req m
                  case fromJSON val of
                    Success gist -> return gist
                    Error err -> error err
