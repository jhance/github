{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | An API for managing 'Blob's on GitHub. A 'Blob' is simply text with an
-- attached checksum computed based on the text for integrity. It is one of the
-- four core Git objects.
--
-- Since 0.1.0.
module Web.GitHub.Object.Blob
    (
    Blob,
    BlobEncoding,
    getBlob
    )
where

import Control.Applicative
import Control.Failure
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Conduit

import Web.GitHub.Internal.Request

-- | A 'Blob' either ready to be created or fetched from 'GitHub'. As such,
-- it is possible (and encouraged) to create these by hand, only many other
-- core data types in this library.
--
-- Since 0.1.0.
data Blob = Blob {
    blobContent :: T.Text,
    blobEncoding :: BlobEncoding
    }
    deriving (Show, Read, Eq)

-- | Blobs can be encoded with either Utf8 or with Base64.
--
-- Since 0.1.0.
data BlobEncoding = BlobEncodingUtf8 | BlobEncodingBase64
    deriving (Show, Read, Eq)

instance FromJSON Blob where
    parseJSON (Object o) = Blob
        <$> o .: "content"
        <*> o .: "encoding"

    parseJSON _ = mzero

instance FromJSON BlobEncoding where
    parseJSON (String s) | s == "utf-8" = return BlobEncodingUtf8
                         | s == "base64" = return BlobEncodingBase64
                         | otherwise = mzero
    parseJSON _ = mzero

-- | Gets the blob from the specified user's repository with the specified
-- checksum.
--
-- Equivalent to @GET https:\/\/api.github.com\/repos\/:user\/:repo\/blobs\/:sha@
--
-- Since 0.1.0.
getBlob :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m,
            MonadThrow m, MonadUnsafeIO m)
        => T.Text -- ^ Username
        -> T.Text -- ^ Repository Name
        -> T.Text -- ^ SHA1 Checksum
        -> Manager
        -> m Blob
getBlob username repository sha m = runResourceT $ do
    req <- parseUrl . T.unpack . T.concat $ [
        "https://api.github.com/repos/",
        username,
        "/",
        repository,
        "/blobs/",
        sha
        ]
    parseValue . fst <$> simpleRequest req m
