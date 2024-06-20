{- |
   Module      : Hochon.Data.Knowledge
   Copyright   : Copyright (C) 2024 barsanges

Connaissances permettant d'exploiter un relevé de compte.
-}

module Hochon.Data.Knowledge
  ( Account(..)
  , Category(..)
  ) where

import Data.Text ( Text )

-- | Identifiant d'un compte bancaire.
newtype Account = Account Text

-- | Catégorie de dépense ou de revenu.
newtype Category = Category Text
