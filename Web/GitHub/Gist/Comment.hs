{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Web.GitHub.Gist.Comment
    (
    -- * Core Data Types
    GistComment(..)
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Control
import Control.Failure
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Network.HTTP.Conduit

import Web.GitHub.Internal.Request
import Web.GitHub.User (PartialUser)

-- | Represents a 'Web.GitHub.Gist.Gist' comment as stored on the GitHub server.
-- The ID is guaranteed to be unique amongst all 'GistComment's on the server,
-- and on the client as well if these are only created by HTTPS requests. If
-- created by hand, the information is likely to be invalid and useless.
data GistComment = GistComment {
    gistCommentBody :: T.Text,
    gistCommentCreatedAt :: T.Text,
    gistCommentId :: Integer,
    gistCommentUrl :: T.Text,
    gistCommentUser :: PartialUser
    }
    deriving (Show, Read, Eq)

instance FromJSON GistComment where
    parseJSON (Object o) = GistComment
        <$> o .: "body"
        <*> o .: "created_at"
        <*> o .: "id"
        <*> o .: "url"
        <*> o .: "user"

    parseJSON _ = mzero

-- | A 'Source' that all of the comments for a particular 'Web.GitHub.Gist.Gist
-- using the 'Web.GitHub.Gist.Gist' ID.
--
-- Equivalent to @GET https:\/\/api.github.com\/gists\/:gist\/comments@.
gistComments :: (Failure HttpException m, MonadBaseControl IO m, MonadResource m)
             => Integer
             -> Manager
             -> Source m GistComment
gistComments gist m = pagedRequest url m $= CL.map parseValue
    where url = "https://api.github.com/gists/" ++ show gist ++ "/coments"
