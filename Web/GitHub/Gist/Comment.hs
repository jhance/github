{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | An API for 'Web.GitHub.Gist.Comment's.
--
-- Since 0.1.0.
module Web.GitHub.Gist.Comment
    (
    GistComment(..),
    gistComments,
    getGistComments,
    getGistComment
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
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
--
-- Since 0.1.0.
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
--
-- Since 0.1.0.
gistComments :: (Failure HttpException m, MonadBaseControl IO m, MonadResource m)
             => Integer
             -> Manager
             -> Source m GistComment
gistComments gist m = pagedRequest url m $= CL.map parseValue
    where url = "https://api.github.com/gists/" ++ show gist ++ "/coments"

-- | Gets all of a a 'Web.GitHub.Gist.Gist's comments. This is a thin wrapper
-- around 'gistComments' which is a more efficient conduit-based version of this
-- same functionality.
--
-- Equivalent to @GET https:\/\/api.github.com\/gists\/:gist\/comments@.
--
-- Since 0.1.0.
getGistComments :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
                    MonadThrow m, MonadUnsafeIO m)
                => Integer
                -> Manager
                -> m [GistComment]
getGistComments gist m = runResourceT $ gistComments gist m $$ CL.consume

-- | Looks up a single 'GistComment' based on its ID.
--
-- Equivalent to @GET https:\/\/api.github.com\/gists\/comments\/:id@.
getGistComment :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
                   MonadThrow m, MonadUnsafeIO m)
               => Integer
               -> Manager
               -> m GistComment
getGistComment i m = runResourceT $ do
    req <- parseUrl $ "https://api.github.com/gists/comments/" ++ show i
    parseValue . fst <$> simpleRequest req m

-- | Creates a new 'GistComment' by using a request to the server.
--
-- Equivalent to @POST https:\/\/api.github.com\/gists\/:gist\/comments@.
createGistComment :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
                      MonadThrow m, MonadUnsafeIO m)
                  => Integer -- ^ ID of parent 'Web.GitHub.Gist.Gist'
                  -> T.Text -- ^ Body of 'GistComment'
                  -> Manager
                  -> m GistComment
createGistComment i body m = runResourceT $ do
    let json = encode . object $ ["body" .= body]
    req <- parseUrl $ "https://api.github.com/gists/" ++ show i ++ "/comments"
    let req' = req { method = "POST", requestBody = RequestBodyLBS json }
    parseValue . fst <$> simpleRequest req m
