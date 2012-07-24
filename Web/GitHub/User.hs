{-# LANGUAGE OverloadedStrings #-}
-- | An API for accessing GitHub 'User's. This has not been implemented yet.
module Web.GitHub.User
    (
    PartialUser(..)
    )
where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.Text as T

-- | Represents a User from the result of a query. This doesn't contain
-- all of the same data as a normal 'User', so to convert another HTTPS query
-- must be made to GitHub to get more detail about the User. To do so, use
-- 'partialUserId'.
--
-- This datatype is needed because queries not part of the 'User' API don't
-- give as much information as is contained in the 'User' datatype.
data PartialUser = PartialUser {
    partialUserLogin :: T.Text,
    partialUserId :: Integer,
    partialUserAvatarUrl :: T.Text,
    partialUserGravatarId :: T.Text,
    partialUserUrl :: T.Text
    }
    deriving (Show, Read, Eq)

instance FromJSON PartialUser where
    parseJSON (Object o) = PartialUser
        <$> o .: "login"
        <*> o .: "id"
        <*> o .: "avatar_url"
        <*> o .: "gravatar_id"
        <*> o .: "url"

    parseJSON _ = mzero
