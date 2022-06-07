{-# LANGUAGE TemplateHaskell #-}
module Arkham.Treachery where

import Arkham.Prelude

import Arkham.Treachery.Attrs hiding (treacheryInHandOf, treacheryOwner)
import Arkham.Treachery.Attrs qualified as Attrs
import Arkham.Treachery.Runner
import Arkham.Treachery.Treacheries
import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Name
import Arkham.Query
import Arkham.Target
import Arkham.Trait (Trait)
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

instance
  ( HasCount PlayerCount env ()
  , HasId LocationId env InvestigatorId
  , HasSet Trait env AssetId
  , HasSet Trait env LocationId
  , HasCount ResourceCount env TreacheryId
  , Query EnemyMatcher env
  , Query InvestigatorMatcher env
  )
  => HasModifiersFor Treachery where
  getModifiersFor = $(entityF2 "Treachery" "getModifiersFor")

instance HasCardCode Treachery where
  toCardCode = toCardCode . toAttrs

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Treachery" "toAttrs")

instance Named Treachery where
  toName = toName . toAttrs

instance TargetEntity Treachery where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Treachery where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Treachery where
  toCardId = toCardId . toAttrs
  toCardOwner = toCardOwner . toAttrs

instance HasModifiersFor env () => HasCount DoomCount env Treachery where
  getCount t = do
    modifiers <- getModifiers (toSource t) (toTarget t)
    let f = if DoomSubtracts `elem` modifiers then negate else id
    pure . DoomCount . f . treacheryDoom $ toAttrs t

instance HasCount ResourceCount env Treachery where
  getCount = getCount . toAttrs

instance HasCount (Maybe ClueCount) env Treachery where
  getCount = pure . (ClueCount <$>) . treacheryClues . toAttrs

lookupTreachery :: CardCode -> (InvestigatorId -> TreacheryId -> Treachery)
lookupTreachery cardCode =
  fromJustNote ("Unknown treachery: " <> pack (show cardCode))
    $ lookup cardCode allTreacheries

allTreacheries :: HashMap CardCode (InvestigatorId -> TreacheryId -> Treachery)
allTreacheries = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Treachery")

isWeakness :: Treachery -> Bool
isWeakness = isJust . cdCardSubType . toCardDef

treacheryInHandOf :: Treachery -> Maybe InvestigatorId
treacheryInHandOf = Attrs.treacheryInHandOf . toAttrs

treacheryOwner :: Treachery -> Maybe InvestigatorId
treacheryOwner = Attrs.treacheryOwner . toAttrs

treacheryTarget :: Treachery -> Maybe Target
treacheryTarget = treacheryAttachedTarget . toAttrs
