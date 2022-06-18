{-# LANGUAGE TemplateHaskell #-}
module Arkham.Treachery where

import Arkham.Prelude

import Arkham.Treachery.Runner
import Arkham.Treachery.Treacheries
import Arkham.Card
import Arkham.Classes
import Arkham.Classes.Entity.TH
import Arkham.Id
import Data.Aeson.TH

$(buildEntity "Treachery")

$(deriveJSON defaultOptions ''Treachery)

createTreachery :: IsCard a => a -> InvestigatorId -> Treachery
createTreachery a iid =
  lookupTreachery (toCardCode a) iid (TreacheryId $ toCardId a)

instance HasCardDef Treachery where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Treachery where
  getAbilities = $(entityF "Treachery" "getAbilities")

instance RunMessage Treachery where
  runMessage = $(entityRunMessage "Treachery")

instance HasModifiersFor Treachery where
  getModifiersFor = $(entityF2 "Treachery" "getModifiersFor")

instance HasCardCode Treachery where
  toCardCode = toCardCode . toAttrs

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Treachery" "toAttrs")

instance TargetEntity Treachery where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Treachery where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Treachery where
  toCardId = toCardId . toAttrs
  toCardOwner = toCardOwner . toAttrs

lookupTreachery :: CardCode -> (InvestigatorId -> TreacheryId -> Treachery)
lookupTreachery cardCode =
  fromJustNote ("Unknown treachery: " <> pack (show cardCode))
    $ lookup cardCode allTreacheries

allTreacheries :: HashMap CardCode (InvestigatorId -> TreacheryId -> Treachery)
allTreacheries = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Treachery")
