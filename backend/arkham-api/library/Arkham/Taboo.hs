module Arkham.Taboo (module Arkham.Taboo, module Arkham.Taboo.Types) where

import Arkham.Card
import Arkham.CommitRestriction
import Arkham.Customization
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Taboo.Types
import Data.Map.Strict qualified as Map
import GHC.Records

tabooListModify :: TabooList -> CardDef -> CardDef
tabooListModify = \case
  TabooList15 -> tabooList15Modify
  TabooList16 -> tabooList16Modify
  TabooList18 -> tabooList18Modify
  TabooList19 -> tabooList19Modify
  TabooList20 -> tabooList20Modify
  TabooList21 -> tabooList21Modify
  TabooList22 -> tabooList22Modify
  TabooList23 -> tabooList23Modify

tabooList15Modify :: CardDef -> CardDef
tabooList15Modify cdef = case toCardCode cdef of
  "02266" -> cdef {cdLimits = [MaxPerRound 1]} -- Ace in the Hole
  "02229" -> cdef {cdCommitRestrictions = [MaxOnePerTest]} -- Quick Thinking
  "03315" -> cdef {cdExceptional = True} -- Key of Ys
  _ -> cdef

tabooList16Modify :: CardDef -> CardDef
tabooList16Modify cdef = case toCardCode cdef of
  "02229" -> cdef {cdCommitRestrictions = [MaxOnePerTest]} -- Quick Thinking
  _ -> tabooList15Modify cdef

tabooList18Modify :: CardDef -> CardDef
tabooList18Modify cdef = case toCardCode cdef of
  "02229" -> cdef -- Quick Thinking, removed restriction
  _ -> tabooList16Modify cdef

tabooList19Modify :: CardDef -> CardDef
tabooList19Modify cdef = case toCardCode cdef of
  _ -> tabooList18Modify cdef

tabooList20Modify :: CardDef -> CardDef
tabooList20Modify cdef = case toCardCode cdef of
  "04110" ->
    cdef
      { cdFastWindow =
          Just
            $ RevealChaosToken #when You
            $ IsSymbol
            <> ChaosTokenFaceIsNot #autofail
      }
  "07268" -> cdef {cdExceptional = False, cdDeckRestrictions = [PerDeckLimit 1]}
  "08055" -> cdef {cdLimits = [MaxPerRound 1]}
  _ -> tabooList19Modify cdef

tabooList21Modify :: CardDef -> CardDef
tabooList21Modify cdef = case toCardCode cdef of
  "08076" -> cdef {cdLimits = [MaxPerGame 2]}
  "09022" -> cdef {cdCustomizations = Map.adjust (+ 1) InscriptionOfTheHunt cdef.customizations}
  _ -> tabooList20Modify cdef

tabooList22Modify :: CardDef -> CardDef
tabooList22Modify cdef = case toCardCode cdef of
  _ -> tabooList21Modify cdef

tabooList23Modify :: CardDef -> CardDef
tabooList23Modify cdef = case toCardCode cdef of
  "08113" -> cdef {cdDeckRestrictions = [CampaignModeOnly]}
  _ -> tabooList22Modify cdef

tabooed :: HasField "taboo" a (Maybe TabooList) => TabooList -> a -> Bool
tabooed tbl = maybe False (>= tbl) . getField @"taboo"
