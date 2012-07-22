{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Web.GitHub.Gist
    (
    Gist(..),
    getGist,
    getGists,
    gists
    )
where

import Control.Applicative
import Control.Failure
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as CL
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

instance FromJSON GistUser where
    parseJSON (Object o) = GistUser
        <$> o .: "login"
        <*> o .: "id"
        <*> o .: "avatar_url"
        <*> o .: "gravatar_id"
        <*> o .: "url"

    parseJSON _ = mzero

jsonToGist :: Value -> Gist
jsonToGist val = case fromJSON val of
                    Success gist -> gist
                    Error err -> error err

-- | Gets a gist by ID.
getGist :: (Failure HttpException m, MonadResource m, MonadBaseControl IO m)
        => Integer 
        -> Manager
        -> m Gist
getGist id m = do req <- parseUrl $ "https://api.github.com/gists/" ++ show id
                  (val, headers) <- simpleRequest req m
                  return $ jsonToGist val

-- | Gets a list of all Gists of a user. Equivalent to
-- 'https://api.github.com/users/:user/gists'
-- 
-- As long as there is a next rel, it will continue to fetch more gists. For
-- more efficiency, use `gists`
getGists :: (Failure HttpException m, MonadResource m, MonadBaseControl IO m)
         => String
         -> Manager
         -> m [Gist]
getGists user m = gists user m $$ CL.consume

-- | Source that obtains all gists of a user with the specified username.
gists :: (Failure HttpException m, MonadResource m, MonadBaseControl IO m)
      => String
      -> Manager
      -> Source m Gist
gists user m= let url = "https://api.github.com/users/" ++ user ++ "/gists"
              in pagedRequest url m $= CL.map jsonToGist
