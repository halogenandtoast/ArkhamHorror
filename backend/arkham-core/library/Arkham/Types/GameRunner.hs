module Arkham.Types.GameRunner where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude

type GameRunner env
  = ( HasQueue env
    , HasCount CardCount InvestigatorId env
    , HasCount ClueCount LocationId env
    , HasCount EnemyCount InvestigatorId env
    , HasCount HealthDamageCount EnemyId env
    , HasCount HorrorCount InvestigatorId env
    , HasCount PlayerCount () env
    , HasCount RemainingSanity InvestigatorId env
    , HasCount ResourceCount InvestigatorId env
    , HasCount SanityDamageCount EnemyId env
    , HasId (Maybe OwnerId) AssetId env
    , HasId (Maybe StoryAssetId) CardCode env
    , HasId (Maybe StoryEnemyId) CardCode env
    , HasId ActiveInvestigatorId () env
    , HasId LeadInvestigatorId () env
    , HasId CardCode EnemyId env
    , HasId LocationId InvestigatorId env
    , HasList DeckCard (InvestigatorId, Trait) env
    , HasModifiers env InvestigatorId
    , HasModifiers env LocationId
    , HasModifiersFor env env
    , HasSet AccessibleLocationId LocationId env
    , HasSet AssetId InvestigatorId env
    , HasSet BlockedLocationId () env
    , HasSet ClosestLocationId (LocationId, LocationId) env
    , HasSet ClosestLocationId (LocationId, Prey) env
    , HasSet ConnectedLocationId LocationId env
    , HasSet EmptyLocationId () env
    , HasSet EnemyId InvestigatorId env
    , HasSet EnemyId LocationId env
    , HasSet EventId () env
    , HasSet EventId LocationId env
    , HasSet HandCardId (InvestigatorId, PlayerCardType) env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId LocationId env
    , HasSet LocationId () env
    , HasSet LocationId [Trait] env
    , HasSet PreyId (Prey, LocationId) env
    , HasSet PreyId Prey env
    , HasSet Trait AssetId env
    , HasSet Trait EnemyId env
    , HasSet Trait LocationId env
    , HasSet TreacheryId LocationId env
    , HasSource ForSkillTest env
    , HasTestAction ForSkillTest env
    )
