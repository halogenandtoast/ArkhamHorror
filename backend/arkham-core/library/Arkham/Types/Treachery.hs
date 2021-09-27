{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Treachery where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait (Trait)
import Arkham.Types.Treachery.Attrs hiding (treacheryInHandOf, treacheryOwner)
import Arkham.Types.Treachery.Attrs qualified as Attrs
import Arkham.Types.Treachery.Cards
import Arkham.Types.Treachery.Runner

$(buildEntity "Treachery")

createTreachery :: IsCard a => a -> InvestigatorId -> Treachery
createTreachery a iid =
  lookupTreachery (toCardCode a) iid (TreacheryId $ toCardId a)

instance HasCardDef Treachery where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Treachery where
  getAbilities = genericGetAbilities

instance TreacheryRunner env => RunMessage env Treachery where
  runMessage = genericRunMessage

instance
  ( HasCount PlayerCount env ()
  , HasId LocationId env InvestigatorId
  , HasId (Maybe OwnerId) env AssetId
  , HasSet Trait env AssetId
  , HasSet Trait env LocationId
  , HasCount ResourceCount env TreacheryId
  , Query EnemyMatcher env
  )
  => HasModifiersFor env Treachery where
  getModifiersFor = genericGetModifiersFor

instance HasCardCode Treachery where
  toCardCode = toCardCode . toAttrs

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs

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

instance HasCount DoomCount env Treachery where
  getCount = pure . DoomCount . treacheryDoom . toAttrs

instance HasCount ResourceCount env Treachery where
  getCount = getCount . toAttrs

instance HasCount (Maybe ClueCount) env Treachery where
  getCount = pure . (ClueCount <$>) . treacheryClues . toAttrs

instance HasId (Maybe OwnerId) env Treachery where
  getId = pure . (OwnerId <$>) . Attrs.treacheryOwner . toAttrs

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

instance CanBeWeakness env Treachery where
  getIsWeakness = pure . isWeakness

treacheryTarget :: Treachery -> Maybe Target
treacheryTarget = treacheryAttachedTarget . toAttrs
