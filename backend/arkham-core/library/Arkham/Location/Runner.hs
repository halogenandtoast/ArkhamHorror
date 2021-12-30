module Arkham.Location.Runner where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Card.Id
import Arkham.Classes
import Arkham.Id
import Arkham.LocationSymbol
import Arkham.Matcher
import Arkham.Name
import Arkham.Query
import Arkham.SkillTest
import Arkham.Source
import Arkham.Trait

type LocationRunner env =
  ( HasQueue env
  , HasSkillTest env
  , HasSet EnemyId env EnemyMatcher
  , Query AssetMatcher env
  , Query LocationMatcher env
  , Query EnemyMatcher env
  , Query ExtendedCardMatcher env
  , HasCostPayment env
  , HasCount ActionRemainingCount env InvestigatorId
  , HasCount ClueCount env InvestigatorId
  , HasCount HorrorCount env InvestigatorId
  , HasCount PlayerCount env ()
  , HasId (Maybe LocationId) env LocationMatcher
  , HasId (Maybe OwnerId) env AssetId
  , HasId ActiveInvestigatorId env ()
  , HasId CardCode env EnemyId
  , HasId LeadInvestigatorId env ()
  , HasId LocationId env EnemyId
  , HasId LocationId env InvestigatorId
  , HasList DiscardedEncounterCard env ()
  , HasList HandCard env InvestigatorId
  , HasList LocationName env ()
  , HasList UsedAbility env ()
  , HasModifiersFor env ()
  , HasName env LocationId
  , HasSet ActId env ()
  , HasSet ConnectedLocationId env LocationId
  , HasSet EnemyId env Trait
  , HasSet EnemyId env CardCode
  , HasSet EnemyAccessibleLocationId env (EnemyId, LocationId)
  , HasSet EventId env ()
  , HasSet HandCardId env (InvestigatorId, CardType)
  , HasSet InvestigatorId env ()
  , HasSet LocationId env ()
  , HasSet LocationId env LocationMatcher
  , HasSet LocationId env (HashSet LocationSymbol)
  , HasSet LocationId env [Trait]
  , HasList SetAsideCard env ()
  , HasSet Trait env Source
  , HasSet Trait env EnemyId
  , HasSet Trait env LocationId
  , HasSet UnrevealedLocationId env ()
  , HasSet UnrevealedLocationId env LocationMatcher
  )
