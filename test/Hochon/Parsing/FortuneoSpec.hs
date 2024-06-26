{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Hochon.Parsing.FortuneoSpec
   Copyright   : Copyright (C) 2024 barsanges

Teste le module 'Hochon.Parsing.Fortuneo'.
-}

module Hochon.Parsing.FortuneoSpec
  ( spec
  ) where

import Test.Hspec

import Data.Map as M
import Data.Text as Tx
import Data.Time as Ti
import Hochon.Data.Entry ( Currency(..), Method(..), Entry(..) )
import Hochon.Data.Knowledge ( Account(..), Category(..) )
import Hochon.Parsing.Fortuneo ( parseFortuneo )

acc :: Account
acc = Account "Fortuneo"

knowledge :: M.Map Tx.Text (Tx.Text, Category)
knowledge = M.fromList [ ("foobar", ("foo", Category "Lorem"))
                       , ("bazqux", ("baz", Category "Ipsum"))
                       ]

spec :: Spec
spec = do
  describe "parseFortuneo" $ do
    it "some entries may benefit from external knowledge" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "25/06/2024;25/06/2024;CARTE 24/06 foobar;-10;"
                                                           , "31/12/2025;31/12/2025;VIR INST bazqux;;23"
                                                           ])
      let expected = Right [ Entry { account = acc
                                   , date = Ti.fromGregorian 2024 6 25
                                   , amount = -10
                                   , category = Just (Category "Lorem")
                                   , label = "foo"
                                   , method = Just DebitCard
                                   , currency = EUR
                                   }
                           , Entry { account = acc
                                   , date = Ti.fromGregorian 2025 12 31
                                   , amount = 23
                                   , category = Just (Category "Ipsum")
                                   , label = "baz"
                                   , method = Just Transfer
                                   , currency = EUR
                                   }
                           ]
      actual `shouldBe` expected

    it "some entries may be parsed without benefitting from external knowledge" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "01/04/2019;01/04/2019;RET DAB 01/04/2019 ICI;-100;"
                                                           , "09/06/2018;09/06/2018;CARTE 08/06 Ainay;-15;"
                                                           ])
      let expected = Right [ Entry { account = acc
                                   , date = Ti.fromGregorian 2019 4 1
                                   , amount = -100
                                   , category = Nothing
                                   , label = "RET DAB 01/04/2019 ICI"
                                   , method = Nothing
                                   , currency = EUR
                                   }
                           , Entry { account = acc
                                   , date = Ti.fromGregorian 2018 6 9
                                   , amount = -15
                                   , category = Nothing
                                   , label = "Ainay"
                                   , method = Just DebitCard
                                   , currency = EUR
                                   }
                           ]
      actual `shouldBe` expected

    it "some entries may benefit from external knowledge, others may not" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "01/04/2019;01/04/2019;RET DAB 01/04/2019 ICI;-100;"
                                                           , "25/06/2024;25/06/2024;CARTE 24/06 foobar;-10;"
                                                           ])
      let expected = Right [  Entry { account = acc
                                   , date = Ti.fromGregorian 2019 4 1
                                   , amount = -100
                                   , category = Nothing
                                   , label = "RET DAB 01/04/2019 ICI"
                                   , method = Nothing
                                   , currency = EUR
                                   }
                           , Entry { account = acc
                                   , date = Ti.fromGregorian 2024 6 25
                                   , amount = -10
                                   , category = Just (Category "Lorem")
                                   , label = "foo"
                                   , method = Just DebitCard
                                   , currency = EUR
                                   }
                           ]
      actual `shouldBe` expected

    it "blank lines are ignored" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ ""
                                                           , "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "   "
                                                           , "01/04/2019;01/04/2019;RET DAB 01/04/2019 ICI;-100;"
                                                           , "09/06/2018;09/06/2018;CARTE 08/06 Ainay;-15;"
                                                           , "\t"
                                                           ])
      let expected = Right [ Entry { account = acc
                                   , date = Ti.fromGregorian 2019 4 1
                                   , amount = -100
                                   , category = Nothing
                                   , label = "RET DAB 01/04/2019 ICI"
                                   , method = Nothing
                                   , currency = EUR
                                   }
                           , Entry { account = acc
                                   , date = Ti.fromGregorian 2018 6 9
                                   , amount = -15
                                   , category = Nothing
                                   , label = "Ainay"
                                   , method = Just DebitCard
                                   , currency = EUR
                                   }
                           ]
      actual `shouldBe` expected

    it "a file with nothing but a header is parsed correctly" $ do
      let actual = parseFortuneo acc knowledge "Date opération;Date valeur;libellé;Débit;Crédit;"
      actual `shouldBe` Right []

    it "a completely empty file raises an error" $ do
      let actual = parseFortuneo acc knowledge ""
      actual `shouldBe` Left ["no data"]

    it "an incorrect header raises an error" $ do
      let actual = parseFortuneo acc knowledge "Crédit;Date opération;Date valeur;libellé;Débit;"
      actual `shouldBe` Left ["unknown data format"]

    it "an incorrect number of fields in a line raises an error" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "09/06/2018;CARTE 08/06 Ainay;-15;"
                                                           ])
      actual `shouldBe` Left ["line 1: incorrect number of fields, expected 5, got 4"]

    it "an erroneous date raises an error (1)" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "04/17/2018;09/06/2018;CARTE 08/06 Ainay;-15;"
                                                           ])
      actual `shouldBe` Left ["line 1: unable to parse the date '04/17/2018'"]

    it "an erroneous date raises an error (2)" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "9 juin 2018;09/06/2018;CARTE 08/06 Ainay;-15;"
                                                           ])
      actual `shouldBe` Left ["line 1: unable to parse the date '9 juin 2018'"]

    it "an erroneous date raises an error (3)" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "foo ?;09/06/2018;CARTE 08/06 Ainay;-15;"
                                                           ])
      actual `shouldBe` Left ["line 1: unable to parse the date 'foo ?'"]

    it "an erroneous debit or credit raises an error (1)" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "09/06/2018;09/06/2018;CARTE 08/06 Ainay;.1;"
                                                           ])
      actual `shouldBe` Left ["line 1: the field '.1' should only contain a number"]

    it "an erroneous debit or credit raises an error (2)" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "09/06/2018;09/06/2018;CARTE 08/06 Ainay;;bar !"
                                                           ])
      actual `shouldBe` Left ["line 1: the field 'bar !' should only contain a number"]

    it "an erroneous debit or credit raises an error (3)" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "09/06/2018;09/06/2018;CARTE 08/06 Ainay;1e2 + 3.14;"
                                                           ])
      actual `shouldBe` Left ["line 1: the field '1e2 + 3.14' should only contain a number"]

    it "several errors may be parsed at once" $ do
      let actual = parseFortuneo acc knowledge (Tx.unlines [ "Date opération;Date valeur;libellé;Débit;Crédit;"
                                                           , "01/04/2019;01/04/2019;RET DAB 01/04/2019 ICI"
                                                           , "04/17/2018;09/06/2018;CARTE 08/06 Ainay;-15;"
                                                           ])
      actual `shouldBe` Left [ "line 1: incorrect number of fields, expected 5, got 3"
                             , "line 2: unable to parse the date '04/17/2018'"
                             ]
