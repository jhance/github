{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | Contains an internal API for creating requests to the GitHub server. The
-- rest of the modules in this package are wrappers around these two functions
-- that automatically deserialize and serialize the JSON requests and responses.
module Web.GitHub.Internal.Request
    (
    pagedRequest,
    simpleRequest
    )
where

import Control.Monad.Trans.Control
import Control.Failure
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.CaseInsensitive (CI)
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Vector as V
import Network.HTTP.Conduit
import Network.HTTP.Types

-- | Requests all of the pages from GitHub. Because GitHub sends JSON in pages,
-- with typically 30 items per-page, it is necessary to send more than one
-- request.
--
-- This takes the parameter as a String and always runs it as a GET request,
-- because pagination is not needed for any other verb.
--
-- Since 0.1.0
pagedRequest :: (Failure HttpException m, MonadResource m, MonadBaseControl IO m)
             => String
             -> Manager
             -> Source m A.Value
pagedRequest r m = sourceState (PNext r) $ pagePull r m

-- | Represents the state of sourcing pages. A vector contains all of the items
-- from a single page.
data PageState = PNext String
                 | PNextVector (V.Vector A.Value) String
                 | PDone
                 | PDoneVector (V.Vector A.Value) 
                 deriving (Eq, Show)

-- | Pulls a single page from GitHub using `simpleRequest`. If available, it
-- uses a url specified with the rel from the header of the previous request
-- to grab the next page. The JSON Value's are the values from within the
-- top-level array.
--
-- Since 0.1.0
pagePull :: (Failure HttpException m, MonadResource m, MonadBaseControl IO m)
         => String
         -> Manager
         -> PageState
         -> m (SourceStateResult PageState A.Value)
pagePull r m (PNext url) = do
    r' <- parseUrl r
    (jsonVal, headers) <- simpleRequest r' m
    let (A.Array jsonArray) = jsonVal
    if V.length jsonArray == 0
        then return $ StateClosed
        else case lookup "link" headers of
            Nothing -> if V.length jsonArray == 1
                then return $ StateOpen PDone (jsonArray V.! 0)
                else return $ StateOpen (PDoneVector $ V.tail jsonArray) (V.head jsonArray)
            Just linkHeader ->
                case findNextLink (BC.unpack linkHeader) of
                    Nothing -> if V.length jsonArray == 1
                        then return $ StateOpen PDone (V.head jsonArray)
                        else return $ StateOpen (PDoneVector $ V.tail jsonArray) (V.head jsonArray)
                    Just nextUrl -> if V.length jsonArray == 1
                        then return $ StateOpen (PNext nextUrl) (V.head jsonArray)
                        else return $ StateOpen (PNextVector (V.tail jsonArray) nextUrl) (V.head jsonArray)

-- vec is guaranteed to have length >= 1
pagePull _ _ (PNextVector vec nextUrl) = do
    if V.length vec == 1
        then return $ StateOpen (PNext nextUrl) (V.head vec)
        else return $ StateOpen (PNextVector (V.tail vec) nextUrl) (V.head vec)
pagePull _ _ (PDoneVector vec) = do
    if V.length vec == 1
        then return $ StateOpen PDone (V.head vec)
        else return $ StateOpen (PDoneVector $ V.tail vec) (V.head vec)

pagePull _ _ PDone = return StateClosed

-- | Finds the link to the next url in the Link header
findNextLink :: String -> Maybe String
findNextLink s = let rels = split (dropDelims $ oneOf ",") s
                     filtered = filter f rels
                     f str = isInfixOf "rel=\"next\"" str
                 in case filtered of
                    [] -> Nothing
                    (x:_) -> Just $ parseNextRel x

-- | Parses a row for its url.
parseNextRel :: String -> String
parseNextRel s = let left = fromJust $ elemIndex '<' s
                     right = fromJust $ elemIndex '>' s
                 in take (right - 1) . drop (left + 1) $ s

-- | A simple request that completely ignores pagination. This should be used
-- for any verb except GET which should use pagedRequest. The response is parsed
-- as a top-level JSON object (this varies from `pagedRequest`, where the
-- values returned are objects from within the top-level array)
-- 
-- Since 0.1.0
simpleRequest :: (MonadResource m, MonadBaseControl IO m)
              => Request m
              -> Manager
              -> m (A.Value, ResponseHeaders)
simpleRequest r m = do response <- http r m
                       let source = responseBody response
                       jsonVal <- source $$ jsonSink
                       let headers = responseHeaders response
                       return (jsonVal, headers)

-- | A simple sink that takes in the ByteString containing JSON from the
-- request and parses it incrementally using Attoparsec. This allows the
-- JSON to be parsed while it is being sent from GitHub.
--
-- Since 0.1.0
jsonSink :: MonadThrow m => Sink B.ByteString m A.Value
jsonSink = sinkParser AP.json
