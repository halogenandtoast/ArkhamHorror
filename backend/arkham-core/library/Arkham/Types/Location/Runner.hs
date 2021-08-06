module Arkham.Types.Location.Runner where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Trait

type LocationRunner env
  = ( HasQueue env
    , Query AssetMatcher env
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
    , HasSet LocationId env (Set LocationSymbol)
    , HasSet LocationId env [Trait]
    , HasList SetAsideCard env ()
    , HasSet Trait env Source
    , HasSet Trait env EnemyId
    , HasSet Trait env LocationId
    , HasSet UnrevealedLocationId env ()
    , HasSet UnrevealedLocationId env LocationMatcher
    )

