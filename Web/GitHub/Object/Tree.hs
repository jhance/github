-- | An API for dealing with Git 'Tree's. A 'Tree' is Gits generic
-- representation of a filesystem. Each 'Tree' defines a single directory
-- (but not its subdirectories), mapping single-level filepaths to other
-- Git objects (either a blob or a tree).
module Web.GitHub.Object.Tree
    (
    Tree(..),
    TreeNode(..)
    )
where

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
    deriving (Show, Read, Eq)

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
    deriving (Show, Read, Eq)
