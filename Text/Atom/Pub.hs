--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Pub
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability:
--
-- Types for the Atom Publishing Protocol (APP)
--
--------------------------------------------------------------------
module Text.Atom.Pub where

import Text.XML.Light.Types as XML
import Text.Atom.Feed ( TextContent, Category, URI )

data Service
 = Service
    { serviceWorkspaces :: [Workspace]
    , serviceOther      :: [XML.Element]
    }

data Workspace
 = Workspace
    { workspaceTitle   :: TextContent
    , workspaceCols    :: [Collection]
    , workspaceOther   :: [XML.Element]
    }

data Collection
 = Collection
    { collectionURI    :: URI
    , collectionTitle  :: TextContent
    , collectionAccept :: [Accept]
    , collectionCats   :: [Categories]
    , collectionOther  :: [XML.Element]
    }

data Categories
 = CategoriesExternal URI
 | Categories (Maybe Bool) (Maybe URI) [Category]
     deriving (Show)

newtype Accept = Accept { acceptType :: String }
