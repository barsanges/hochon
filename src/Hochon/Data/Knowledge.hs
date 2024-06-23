{- |
   Module      : Hochon.Data.Knowledge
   Copyright   : Copyright (C) 2024 barsanges

Connaissances permettant d'exploiter des relevés de compte.
-}

module Hochon.Data.Knowledge
  ( Account(..)
  , Category(..)
  , Knowledge
  , toKnowledge
  , access
  ) where

import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Map as M

-- | Identifiant d'un compte bancaire.
newtype Account = Account Text
  deriving (Eq, Ord, Show)

-- | Catégorie de dépense ou de revenu.
newtype Category = Category Text
  deriving (Eq, Ord, Show)

-- | Connaissances permettant d'exploiter des relevés de compte.
newtype Knowledge = Knowledge (M.Map Account (M.Map Text (Text, Category)))
  deriving Show

-- | Transforme un dictionnaire de connaissances en un objet 'Knowledge'.
toKnowledge :: M.Map Account (M.Map Text (Text, Category)) -> Knowledge
toKnowledge x = Knowledge x

-- | Accède aux connaissances associées à un compte donné. Ces connaissances
-- peuvent être inexistantes : le dictionnaire renvoyé par la fonction est
-- alors vide.
access :: Account -> Knowledge -> M.Map Text (Text, Category)
access a (Knowledge k) = fromMaybe M.empty (M.lookup a k)
