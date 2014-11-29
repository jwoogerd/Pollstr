{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Language.Syntax where

import Data.Data
import Data.Aeson
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

{- Pollstr abstract syntax -}

type ID         = String
type Title      = String

data Meta = Meta { title       :: Maybe String
                 , author      :: Maybe String
                 , description :: Maybe String
                 } deriving (Show, Eq, Typeable, Data)

data Survey     = Survey ID Meta [Decl] [Section]
                  deriving (Show, Eq, Typeable, Data)

data Section    = Section ID Title [Item] deriving (Show, Eq, Typeable, Data)

data Decl       = QuestDecl ID Question
                | RespDecl ID Response deriving (Show, Eq, Typeable, Data)

data Item       = Item ID Question Response Skip
                  deriving (Show, Eq, Typeable, Data)

data Question   = Question String | Qvar ID deriving (Show, Eq, Typeable, Data)

data Response   = Response [String] | Rvar ID
                  deriving (Show, Eq, Typeable, Data)

data Skip       = Skip ID Response | None deriving (Show, Eq, Typeable, Data)


instance Lift Survey where
    lift s = dataToExpQ (\x -> Nothing) s

instance ToJSON Survey where
    toJSON (Survey id meta decls sections) =
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

instance ToJSON Item where
    toJSON (Item id (Question q) (Response r) None) = 
        object[ "id" .= id, 
                "question" .= q,
                "response" .= toJSON r
              ]
    toJSON (Item id (Question q) (Response r) (Skip to (Response resp))) = 
        object[ "id" .= id, 
                "question" .= q,
                "response" .= (toJSON r),
                "skips"    .= object ["resp" .= toJSON resp, "to" .= to]
              ]
    toJSON _ = undefined

