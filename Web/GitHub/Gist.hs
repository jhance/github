{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | An API for creating and fetching Gists from GitHub. Most of the functions
-- here have very generic types over monad typeclasses; it may be helpful to
-- note that for the most part either 'ResourceT' 'IO' or 'IO' will fit
-- depending on if the resulting monad needs to be a 'MonadResource' or not,
-- respectively.
module Web.GitHub.Gist
    (
    -- * Core Data Types
    Gist(..),
    GistCreate(..),
    GistEdit(..),
    GistFile(..),
    GistUser(..),

    -- * Gists
    createGist,
    deleteGist,
    editGist,
    forkGist,
    getGist,

    -- * User & Public Gists
    gists,
    getGists,
    publicGists,

    -- * Stars
    checkGistStar,
    starGist,
    unstarGist
    )
where

import Control.Applicative
import Control.Failure
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Network.HTTP.Conduit

import Web.GitHub.Internal.Request

-- | Represents a complete Gist as obtained from the JSON sent back by a
-- HTTPS request. This should rarely if ever be created without a HTTPS
-- request, since the information is likely to be invalid.
--
-- As long as the 'Gist' was created a GitHub API request, the ID is guaranteed
-- to be completely unique amongst all 'Gist's on the server.
data Gist = Gist {
    gistUrl :: T.Text,
    gistId :: Integer,
    gistDescription :: Maybe T.Text,
    gistUser :: Maybe GistUser,
    gistCommentCount :: Int,
    gistHtmlUrl :: T.Text,
    gistPullUrl :: T.Text,
    gistPushUrl :: T.Text,
    gistCreatedAt :: T.Text,
    gistFiles :: M.Map T.Text GistFile
    }
    deriving (Show, Read, Eq)

-- | Represents a User from the result of a Gist query. This doesn't contain
-- all of the same data as a normal User, so to convert another HTTPS query
-- must be made to GitHub to get more detail about the User. To do so, use
-- 'gistUserId' and use the GitHub User API.
data GistUser = GistUser {
    gistUserLogin :: T.Text,
    gistUserId :: Integer,
    gistUserAvatarUrl :: T.Text,
    gistUserGravatarId :: T.Text,
    gistUserUrl :: T.Text
    }
    deriving (Show, Read, Eq)

-- | Represents a file stored in a 'Gist' that has already been saved on the
-- server. This data is generated by the server when the 'Gist' is created
-- and should not generally be created by hand.
--
-- If the content is not specified within the JSON, then it becomes Nothing.
-- This does not signify an empty file; rather, the file must be obtained using
-- an HTTP query on 'gistFileUrl'.
--
-- This is more efficient in most cases anyway, since the file may be large, in
-- which case it is best to use conduits to fetch the result in chunks anyway.
-- Hence, the storage of the content is more of a convenience for the case in
-- which the content is sent with the JSON.
data GistFile = GistFile {
    gistFileContent :: Maybe T.Text,
    gistFileLanguage :: Maybe T.Text,
    gistFileName :: T.Text,
    gistFileSize :: Integer,
    gistFileUrl :: T.Text
    }
    deriving (Show, Read, Eq)

-- | Represents a 'Gist' ready for creation. As such, data to be generated by
-- the GitHub server is not included in this data structure.
data GistCreate = GistCreate {
    gistCreateDescription :: Maybe T.Text,
    gistCreatePublic :: Bool,
    gistCreateFiles :: M.Map T.Text T.Text
    }
    deriving (Show, Read, Eq)

-- | Represents an update to a 'Gist'. Fields with value of Nothing will not
-- be updated; every field is optional.
data GistEdit = GistEdit {
    gistEditDescription :: Maybe T.Text,
    gistEditFiles :: Maybe (M.Map T.Text (Maybe T.Text, Maybe T.Text)) -- ^ A map of the form old name to (contents, new name)
    }
    deriving (Show, Read, Eq)

instance FromJSON Gist where
    parseJSON (Object o) = Gist
        <$> o .: "url"
        <*> liftM read (o .: "id")
        <*> o .:? "description"
        <*> o .:? "user"
        <*> o .: "comments"
        <*> o .: "html_url"
        <*> o .: "git_pull_url"
        <*> o .: "git_push_url"
        <*> o .: "created_at"
        <*> o .: "files"

    parseJSON _ = mzero

instance FromJSON GistUser where
    parseJSON (Object o) = GistUser
        <$> o .: "login"
        <*> o .: "id"
        <*> o .: "avatar_url"
        <*> o .: "gravatar_id"
        <*> o .: "url"

    parseJSON _ = mzero

instance FromJSON GistFile where
    parseJSON (Object o) = GistFile
        <$> o .:? "content"
        <*> o .:? "language"
        <*> o .: "filename"
        <*> o .: "size"
        <*> o .: "raw_url"

    parseJSON _ = mzero

instance ToJSON GistCreate where
    toJSON (GistCreate desc pub files) = object $ descPair ++ publicPair ++ filesPair
        where descPair = case desc of 
                            Nothing -> []
                            Just desc' -> ["description" .= desc']
              publicPair = ["public" .= pub]
              filesPair = ["files" .= filesObj]

              filesObj = object $ M.foldrWithKey f [] files
              f filename content acc = [filename .= object ["content" .= content]] ++ acc

instance ToJSON GistEdit where
    toJSON (GistEdit desc files) = object $ descPair ++ filesPair
        where descPair = case desc of
                Nothing -> []
                Just desc' -> ["description" .= desc']
              filesPair = case files of
                Nothing -> []
                Just files' -> ["files" .= object (M.foldrWithKey f [] files')]
              f filename (content, newname) acc = contentPair ++ newnamePair ++ acc
                where contentPair = case content of
                        Nothing -> []
                        Just content' -> ["content" .= content']
                      newnamePair = case newname of
                        Nothing -> []
                        Just newname' -> ["filename" .= newname']

-- | Internal utility function for easily converting a JSON Value to a Gist.
-- Used on the result from a 'simpleRequest' or on the individual values sent
-- by the source 'pagedRequest'.
jsonToGist :: Value -> Gist
jsonToGist val = case fromJSON val of
                    Success gist -> gist
                    Error err -> error err

-- | Creates a new 'Gist' based on the information from a 'GistCreate'. The new
-- 'Gist' is created by a request and sent back in JSON and parsed. This
-- essentially converts a 'GistCreate' template into a real 'Gist' with all of
-- its associated information.
--
-- Equivalent to `POST https://api.github.com/gists`.
createGist :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
               MonadThrow m, MonadUnsafeIO m)
           => GistCreate
           -> Manager
           -> m Gist
createGist gc m = runResourceT $ do
    let json = encode gc
    req <- parseUrl "https://api.github.com/gists"
    let req' = req { method = "POST", requestBody = RequestBodyLBS json }
    (val, _) <- simpleRequest req' m
    return $ jsonToGist val

-- | Deletes the 'Gist' with the given ID.
--
-- Equivalent to `DELETE https://api.github.com/gists/:id`
deleteGist :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
               MonadThrow m, MonadUnsafeIO m)
           => Integer
           -> Manager
           -> m ()
deleteGist i m = runResourceT $ do
    req <- parseUrl $ "https://api.github.com/gists/" ++ show i
    let req' = req { method = "DELETE" }
    simpleRequest req m
    return ()

-- | Edits a 'Gist' with the given id based on the fields available in a
-- 'GistEdit'.
--
-- Equivalent to `PATCH https://api.github.com/gists/:id`
editGist :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
             MonadThrow m, MonadUnsafeIO m)
         => Integer
         -> GistEdit
         -> Manager
         -> m Gist
editGist i ge m = runResourceT $ do
    let json = encode ge
    req <- parseUrl $ "https://api.github.com/gists/" ++ show i
    let req' = req { method = "PATCH", requestBody = RequestBodyLBS json }
    (val, _) <- simpleRequest req' m
    return $ jsonToGist val

-- | Forks a 'Gist', creating a new 'Gist' with a different unique ID.
--
-- Equivalent to `POST https://api.github.com/gists/:id/fork"
forkGist :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
             MonadThrow m, MonadUnsafeIO m)
         => Integer
         -> Manager
         -> m Gist
forkGist i m = runResourceT $ do
    req <- parseUrl $ "https://api.github.com/gists/:id/fork"
    let req' = req { method = "POST" }
    (val, _) <- simpleRequest req' m
    return $ jsonToGist val

-- | Gets a gist by ID.
--
-- Equivalent to `GET https://api.github.com/gists/:id"
getGist :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
            MonadThrow m, MonadUnsafeIO m)
        => Integer 
        -> Manager
        -> m Gist
getGist id m = runResourceT $ do
    req <- parseUrl $ "https://api.github.com/gists/" ++ show id
    (val, _) <- simpleRequest req m
    return $ jsonToGist val

-- | Source that obtains all gists of a user with the specified username. If
-- the user is logged in, then it is able to grab all gists; otherwise only
-- public gists will be fetched. This is equivalent to
-- `https://api.github.com/users/:user/gists'
gists :: (Failure HttpException m, MonadBaseControl IO m, MonadResource m)
      => String
      -> Manager
      -> Source m Gist
gists user m = let url = "https://api.github.com/users/" ++ user ++ "/gists"
               in pagedRequest url m $= CL.map jsonToGist

-- | Gets a list of all Gists of a user.
-- 
-- As long as there is a next rel, it will continue to fetch more gists. For
-- more efficiency, use `gists`.
--
-- This is a small wrapper around the `gists` function.
getGists :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
             MonadThrow m, MonadUnsafeIO m)
         => String
         -> Manager
         -> m [Gist]
getGists user m = runResourceT $ gists user m $$ CL.consume

-- | Source that obtains all public gists from all users. This is equivalent
-- to `GET https://api.github.com/gists/public` or, if the user is not
-- authenticated, `GET https://api.github.com/gists`, although this behavior
-- is not relied upon.
publicGists :: (Failure HttpException m, MonadBaseControl IO m, MonadResource m)
            => Manager
            -> Source m Gist
publicGists m = let url = "https://api.github.com/gists/public"
                in pagedRequest url m $= CL.map jsonToGist

-- | Checks to see if a 'Gist' is starred based on its ID, based on the header
-- response. A response of 204 No Content indicates there is a star, while a
-- response of 404 Not Found indicates that there is no star.
--
-- Equivalent to `POST https://api.github.com/gists/:id/star`.
checkGistStar :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
                  MonadThrow m, MonadUnsafeIO m)
              => Integer
              -> Manager
              -> m Bool
checkGistStar i m = runResourceT $ do
   req <- parseUrl $ "https://api.github.com/gists/" ++ show i ++ "/star"
   (_, headers) <- simpleRequest req m
   case lookup "status" headers of
    Nothing -> error "checkGistStar: No HTTP Status in Response Headers"
    Just status | "204" `B.isInfixOf` status -> return True
                | "404" `B.isInfixOf` status -> return False
                | otherwise -> error $ "checkGistStar: Unknown HTTP Status. Should be 204 or 404, but header was: " ++ BC.unpack status

-- | Marks a 'Gist' as being starred based on its ID.
--
-- Equivalent to `PUT https://api.github.com/gists/:id/star`.
starGist :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
             MonadThrow m, MonadUnsafeIO m)
         => Integer
         -> Manager
         -> m ()
starGist i m = runResourceT $ do
    req <- parseUrl $ "https://api.github.com/gists/" ++ show i ++ "/star"
    let req' = req { method = "PUT" }
    simpleRequest req' m
    return ()

-- | Marks a 'Gist' as being not starred based on its ID.
--
-- Equivalent to `DELETE https://api.github.com/gists/:id/star`.
unstarGist :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
               MonadThrow m, MonadUnsafeIO m)
           => Integer
           -> Manager
           -> m ()
unstarGist i m = runResourceT $ do
    req <- parseUrl $ "https://api.github.com/gists/" ++ show i ++ "/star"
    let req' = req { method = "DELETE" }
    simpleRequest req' m
    return ()
