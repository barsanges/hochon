{- |
   Module      : Hochon.Data.Entry
   Copyright   : Copyright (C) 2024 barsanges

Entrée d'un relevé de compte consolidé.
-}

module Hochon.Data.Entry
  ( Currency(..)
  , Method(..)
  , Entry(..)
  ) where

import Data.Text ( Text )
import Data.Time ( Day )
import Hochon.Data.Knowledge ( Account, Category )

-- | Devise.
data Currency = EUR
  deriving (Eq, Show)

-- | Méthode de paiement.
data Method = Cash
            | Cheque
            | DebitCard
            | DirectDebit
            | Transfer
  deriving (Eq, Show)

-- | Entrée d'un relevé de comptes consolidé.
data Entry = Entry  { account :: Account
                    , date :: Day
                    , amount :: Rational
                    , category :: Maybe Category
                    , label :: Text
                    , method :: Maybe Method
                    , currency :: Currency
                    }
  deriving (Eq, Show)
