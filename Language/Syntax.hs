{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Language.Syntax where

import Data.Data
import Data.Aeson
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

{- Pollstr abstract syntax -}

type ID         = String
type Title      = String
type Options    = [String]

data Meta = Meta { title       :: Maybe String
                 , author      :: Maybe String
                 , description :: Maybe String
                 } deriving (Show, Eq, Typeable, Data)

data Survey     = Survey ID Meta [Section]
                  deriving (Show, Eq, Typeable, Data)

data Section    = Section ID Title [Item] deriving (Show, Eq, Typeable, Data)

data Item       = Item ID Question Response Skip
                  deriving (Show, Eq, Typeable, Data)

data Question   = Question String deriving (Show, Eq, Typeable, Data)

data Response   = Single Options | Multi Options  -- | Free
                  deriving (Show, Eq, Typeable, Data)

data Skip       = Skip ID Response | None deriving (Show, Eq, Typeable, Data)

data Decl       = QuestDecl ID Question
                | RespDecl ID Response deriving (Show, Eq)


instance Lift Survey where
    lift s = dataToExpQ (\x -> Nothing) s

instance ToJSON Survey where
    toJSON (Survey id meta sections) =
        object ["meta" .= toJSON meta, "sections" .= toJSON sections]

instance ToJSON Meta where
  toJSON (Meta title author description) =
    let mkString (Just s) = s
        mkString Nothing  = ""
    in object ["title"       .= mkString title,
               "author"      .= mkString author,
               "description" .= mkString description
              ]

instance ToJSON Section where
    toJSON (Section id title items) =
        object ["title" .= title, "items" .= toJSON items]

instance ToJSON Response where
    toJSON (Single opts) = object["single" .= opts]
    toJSON (Multi opts)  = object["multi"  .= opts]
    
instance ToJSON Item where
    toJSON (Item id (Question q) response None) = 
        object[ "id" .= id, 
                "question" .= q,
                "response" .= toJSON response
              ]
    toJSON (Item id (Question q) response (Skip to (Single opts))) = 
        object[ "id" .= id, 
                "question" .= q,
                "response" .= toJSON response,
                "skips"    .= object ["resp" .= toJSON opts, "to" .= to]
              ]

