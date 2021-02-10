module Arkham.Types.Location.Runner where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Asset.Uses
import Arkham.Types.Trait

type LocationRunner env
  = ( HasQueue env
    , HasCostPayment env
    , HasCount HorrorCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe OwnerId) env AssetId
    , HasId (Maybe StoryAssetId) env CardCode
    , HasId ActiveInvestigatorId env ()
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env InvestigatorId
    , HasList HandCard env InvestigatorId
    , HasList LocationName env ()
    , HasList UsedAbility env ()
    , HasModifiersFor env ()
    , HasName env LocationId
    , HasSet ActId env ()
    , HasSet AssetId env (InvestigatorId, UseType)
    , HasSet AssetId env InvestigatorId
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env Trait
    , HasSet EventId env ()
    , HasSet HandCardId env (InvestigatorId, PlayerCardType)
    , HasSet InvestigatorId env ()
    , HasSet LocationId env ()
    , HasSet LocationId env LocationMatcher
    , HasSet LocationId env [Trait]
    , HasSet Trait env Source
    , HasSet Trait env EnemyId
    , HasSet Trait env LocationId
    , HasSet UnrevealedLocationId env ()
    , HasSet UnrevealedLocationId env LocationMatcher
    )

