module Arkham.Types.Location.Runner where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Trait

type LocationRunner env
  = ( HasQueue env
    , HasCostPayment env
    , HasCount ActionRemainingCount env InvestigatorId
    , HasCount ClueCount env InvestigatorId
    , HasCount HorrorCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe OwnerId) env AssetId
    , HasId (Maybe StoryAssetId) env CardCode
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
    , HasName env SetAsideLocationCardCode
    , HasSet ActId env ()
    , HasSet AssetId env (InvestigatorId, UseType)
    , HasSet AssetId env InvestigatorId
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
    , HasSet SetAsideLocationCardCode env ()
    , HasSet Trait env Source
    , HasSet Trait env EnemyId
    , HasSet Trait env LocationId
    , HasSet UnrevealedLocationId env ()
    , HasSet UnrevealedLocationId env LocationMatcher
    )

