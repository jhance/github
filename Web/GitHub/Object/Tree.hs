{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | An API for dealing with Git 'Tree's. A 'Tree' is Gits generic
-- representation of a filesystem. Each 'Tree' defines a single directory
-- (but not its subdirectories), mapping single-level filepaths to other
-- Git objects (either a blob or a tree).
module Web.GitHub.Object.Tree
    (
    Tree(..),
    TreeNode(..),
    RecursiveTree(..),
    RecursiveTreeFile(..)
    )
where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Text as T

-- | Represents a Git 'Tree'. A 'Tree' is essentially the representation of
-- a directory in Git, although it does not know of its own name as it can
-- have more than one name within various other trees (if more than one tree
-- were to have a reference to this tree).
--
-- At its core, a tree is a mapping from a path (which is a single-level path,
-- and does not contain subdirectories or subfiles) to either a blob or a
-- tree, which is represented with the 'TreeNode' type.
data Tree = Tree {
    treeSha :: T.Text,
    treeUrl :: T.Text,
    treePaths :: M.Map T.Text TreeNode
    }
    deriving (Eq, Ord, Read, Show)

-- | Represents a single element of a 'Tree'. This element contains the
-- filesystem information about another object, which can be either a blob or
-- a tree. For blobs, the node represents a file, while for trees, it represents
-- a directory. The file path is not contained within these, as it is instead
-- inside the parent tree.
data TreeNode = FileNode {
    treeFileExecutable :: Bool,
    treeFileSha :: T.Text,
    treeFileSize :: Integer,
    treeFileUrl :: T.Text
    }
    | DirectoryNode {
    treeDirectorySha :: T.Text,
    treeDirectoryUrl :: T.Text
    }
    deriving (Eq, Ord, Read, Show)

-- | A data structure similar to a 'Tree' but instead of holding all subtrees
-- and blobs of the tree, it holds all subblobs from recursively scanning
-- all subtrees. It contains 'RecursiveTreeFile's instead of 'TreeNode's. This
-- is the only difference between 'Tree' and 'RecursiveTree'.
--
-- A 'RecursiveTree' notably does not contain any information about any
-- subtrees, so the information held by this data type is much different
-- than the information held by the 'Tree' object.
--
-- Empty directories are not supported by Git so all possible subtree paths
-- are able to be found by iterating over the filepaths.
--
-- A 'M.Map' is calculated from what is originally an associative 'V.Vector',
-- so if iteration over all files and paths is desired this may be an extra
-- and useless step. However, without this step, it is impossible to look up
-- a file in less than O(n) time. This solution is not ideal for the case of
-- iteration, but the extra processing does not have too much overhead.
data RecursiveTree = RecursiveTree {
    recursiveTreeSha :: T.Text,
    recursiveTreeUrl :: T.Text,
    recursiveTreePaths :: M.Map T.Text RecursiveTreeFile
    }
    deriving (Eq, Ord, Read, Show)

-- | A 'RecursiveTreeFile' differs from 'TreeNode' in that it always represents
-- a file and its path is from the tree that it was recursively calculated
-- with respect to. As such, a file in the git repository can have more than
-- one associated 'RecursiveTreeFile' based on which 'RecursiveTree' was used
-- to calculate it.
--
-- The path is a relative path but is allowed to contain directory traversal.
-- It can be relative to many different locations so it is important not to
-- assume that it is relative to the repository root unless you can guarantee
-- this.
--
-- The best way to guarantee that it is relative to the root is to obtain the
-- 'Tree' SHA from some 'Commit', and then request the 'RecursiveTree' for that
-- 'Tree' SHA. Then, the list of 'RecursiveTreeFile's will all have all the
-- possible files.
data RecursiveTreeFile = RecursiveTreeFile {
    recursiveTreeFileExecutable :: Bool,
    recursiveTreeFileSha :: T.Text,
    recursiveTreeFileSize :: Integer,
    recursiveTreeFileUrl :: T.Text
    }
    deriving (Eq, Ord, Read, Show)

instance FromJSON Tree where
    parseJSON (Object o) = Tree
        <$> o .: "sha"
        <*> o .: "url"
        <*> (o .: "tree" >>= parseTreeList)
        where parseTreeList :: Value -> Parser (M.Map T.Text TreeNode)
              parseTreeList (Array o') = F.foldlM f M.empty o'
              f :: M.Map T.Text TreeNode -> Value -> Parser (M.Map T.Text TreeNode)
              f acc node@(Object o'') = M.insert
                <$> o'' .: "path"
                <*> parseJSON node
                <*> return acc

instance FromJSON TreeNode where
    parseJSON (Object o) = do
        t <- o .: "type"
        if t == ("blob" :: T.Text)
            then FileNode
                <$> (o .: "mode" >>= return . (== ("100755" :: T.Text)))
                <*> o .: "sha"
                <*> o .: "size"
                <*> o .: "url"
            else DirectoryNode
                <$> o .: "sha"
                <*> o .: "url"

instance FromJSON RecursiveTree where
    parseJSON (Object o) = RecursiveTree
        <$> o .: "sha"
        <*> o .: "url"
        <*> (o .: "tree" >>= parseTreeList)
        where parseTreeList :: Value -> Parser (M.Map T.Text RecursiveTreeFile)
              parseTreeList (Array o') = F.foldlM f M.empty o'
              f :: M.Map T.Text RecursiveTreeFile
                -> Value 
                -> Parser (M.Map T.Text RecursiveTreeFile)
              f acc file@(Object o'') = M.insert
                <$> o'' .: "path"
                <*> parseJSON file
                <*> return acc

instance FromJSON RecursiveTreeFile where
    parseJSON (Object o) = RecursiveTreeFile
        <$> (o .: "mode" >>= return . (== ("100755" :: T.Text)))
        <*> o .: "sha"
        <*> o .: "size"
        <*> o .: "url"
