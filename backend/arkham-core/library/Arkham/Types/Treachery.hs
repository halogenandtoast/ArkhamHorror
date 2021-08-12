{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Treachery where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait (Trait)
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Cards
import Arkham.Types.Treachery.Runner

$(buildEntity "Treachery")

createTreachery :: IsCard a => a -> InvestigatorId -> Treachery
createTreachery a iid =
  lookupTreachery (toCardCode a) iid (TreacheryId $ toCardId a)

instance HasCardDef Treachery where
  toCardDef = toCardDef . toAttrs

instance HasActions Treachery where
  getActions = genericGetActions

instance
  ( GetCardDef env LocationId
  , HasId (Maybe OwnerId) env AssetId
  , HasSet FarthestLocationId env (InvestigatorId, LocationMatcher)
  , HasSet ClosestLocationId env (InvestigatorId, LocationMatcher)
  , HasSet EnemyId env EnemyMatcher
  , HasList UnderneathCard env InvestigatorId
  , HasList DeckCard env InvestigatorId
  , TreacheryRunner env
  , HasSet SkillId env SkillMatcher
  , HasSet EventId env EventMatcher
  , HasSet ClassSymbol env InvestigatorId
  )
  => RunMessage env Treachery where
  runMessage = genericRunMessage

instance
  ( HasCount PlayerCount env ()
  , HasId LocationId env InvestigatorId
  , HasId (Maybe OwnerId) env AssetId
  , HasSet Trait env AssetId
  , HasSet Trait env LocationId
  , HasSet UniqueEnemyId env ()
  , HasCount ResourceCount env TreacheryId
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
  getId = pure . (OwnerId <$>) . treacheryOwner . toAttrs

lookupTreachery :: CardCode -> (InvestigatorId -> TreacheryId -> Treachery)
lookupTreachery cardCode =
  fromJustNote ("Unknown treachery: " <> pack (show cardCode))
    $ lookup cardCode allTreacheries

allTreacheries :: HashMap CardCode (InvestigatorId -> TreacheryId -> Treachery)
allTreacheries = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Treachery")

isWeakness :: Treachery -> Bool
isWeakness = cdWeakness . toCardDef

instance CanBeWeakness env Treachery where
  getIsWeakness = pure . isWeakness

treacheryTarget :: Treachery -> Maybe Target
treacheryTarget = treacheryAttachedTarget . toAttrs
