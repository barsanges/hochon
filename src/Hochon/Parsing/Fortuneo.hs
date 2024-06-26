{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- |
   Module      : Hochon.Parsing.Fortuneo
   Copyright   : Copyright (C) 2024 barsanges

Lit un relevé de compte Fortuneo.
-}

module Hochon.Parsing.Fortuneo
  ( parseFortuneo
  ) where

import Data.Either ( partitionEithers )
import qualified Data.Map as M
import qualified Data.Text as Tx
import qualified Data.Text.Read as Tx.Read
import qualified Data.Time as Ti
import Hochon.Data.Entry ( Currency(..), Method(..), Entry(..) )
import Hochon.Data.Knowledge ( Account, Category )

-- | Lit un relevé de compte Fortuneo.
parseFortuneo :: Account
              -> M.Map Tx.Text (Tx.Text, Category)
              -> Tx.Text
              -> Either [Tx.Text] [Entry]
parseFortuneo acc knowledge raw = case length raw' of
  0 -> Left ["no data"]
  _ -> if head raw' /= "Date opération;Date valeur;libellé;Débit;Crédit;"
       then Left ["unknown data format"]
       else if length errors == 0
            then Right values
            else Left errors
    where
      parsed = fmap (parseLine acc knowledge) (tail raw')
      parsedWithNumber = fmap addLineNumber (zip [1..(length raw')] parsed)
      (errors, values) = partitionEithers parsedWithNumber
       -- TODO : plutôt que de tout mettre dans une string opaque, il faudrait un type d'erreur...
  where
    raw' = Tx.lines raw

-- | Ajoute un numéro de ligne à une erreur éventuelle.
addLineNumber :: (Int, Either Tx.Text a) -> Either Tx.Text a
addLineNumber (i, Left txt) = Left $ Tx.concat [ "line "
                                               , Tx.pack $ show i
                                               , ": "
                                               , txt
                                               ]
addLineNumber (_, Right x) = Right x

-- | Lit une ligne d'un relevé de compte Fortuneo.
parseLine :: Account
          -> M.Map Tx.Text (Tx.Text, Category)
          -> Tx.Text
          -> Either Tx.Text Entry
parseLine acc knowledge line =
  if length fields /= 5
  then Left $ Tx.pack $ "incorrect number of fields, expected 5, got " ++ show (length fields)
  else do
    d <- parseDay (fields !! 0)
    let (m, banklabel) = splitMethod (fields !! 2)
    let (l, c) = case M.lookup banklabel knowledge of
          Just (customlabel, cat) -> (customlabel, Just cat)
          Nothing -> (banklabel, Nothing)
    x <- parseAmount (fields !! 3)
    y <- parseAmount (fields !! 4)
    return Entry { account = acc
                 , date = d
                 , amount = x + y
                 , category = c
                 , label = l
                 , method = m
                 , currency = EUR
                 }
  where
    fields = Tx.split (\ x -> x == ';') line

instance MonadFail (Either Tx.Text) where
    fail = Left . Tx.pack

-- | Lit une date au format "JJ/MM/AAAA".
parseDay :: Tx.Text -> Either Tx.Text Ti.Day
parseDay = (Ti.parseTimeM False Ti.defaultTimeLocale "%d/%m/%Y") . Tx.unpack

-- | Extrait, dans le libellé Fortuneo, le moyen de paiement.
splitMethod :: Tx.Text -> (Maybe Method, Tx.Text)
splitMethod t = case getFirstJust fs t of
  Just (m, l) -> (Just m, l)
  Nothing -> (Nothing, t)
  where
    -- TODO : traiter le cas "RET DAB"
    fs = [debitcard, directdebit, transfer]

-- | Applique séquentiellement des fonctions à une entrée et renvoie le premier
-- résultat qui n'est pas 'Nothing'.
getFirstJust :: [a -> Maybe b] -> a -> Maybe b
getFirstJust [] _ = Nothing
getFirstJust (f:fs) x = case f x of
  Just y -> Just y
  Nothing -> getFirstJust fs x

-- | Identifie une ligne correspondant à un paiement par carte.
debitcard :: Tx.Text -> Maybe (Method, Tx.Text)
debitcard t = do
  l <- Tx.stripSuffix "CARTE " t
  return (DebitCard, Tx.drop 6 l) -- On supprime l'info inutile "JJ/MM ".

-- | Identifie une ligne correspondant à un prélèvement.
directdebit :: Tx.Text -> Maybe (Method, Tx.Text)
directdebit t = do
  l <- Tx.stripSuffix "PRLV " t
  return (DirectDebit, l)

-- | Identifie une ligne correspondant à un virement ou un virement instantané.
transfer :: Tx.Text -> Maybe (Method, Tx.Text)
transfer t = case Tx.stripSuffix "VIR " t of
  Just t' -> case Tx.stripSuffix "INST " t' of
    Just t'' -> Just (Transfer, t'') -- "VIR INST "
    Nothing -> Just (Transfer, t') -- "VIR "
  Nothing -> Nothing

-- | Lit un chiffre écrit avec une virgule comme séparateur décimal.
parseAmount :: Tx.Text -> Either Tx.Text Rational
parseAmount txt = case Tx.Read.rational $ Tx.replace "," "." txt of
  Left e -> Left $ Tx.pack e
  Right (x, "") -> Right x
  Right (_, _) -> Left $ Tx.concat [ "the field '"
                                   , txt
                                   , "' should only contain a number"
                                   ]
