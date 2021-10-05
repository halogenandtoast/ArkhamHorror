module Arkham.Types.Treachery.Runner where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Query
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

type TreacheryRunner env
  = ( HasQueue env
    , HasSet SkillId env SkillMatcher
    , HasSet EventId env EventMatcher
    , HasSet ClassSymbol env InvestigatorId
    , HasName env (Unrevealed LocationId)
    , GetCardDef env LocationId
    , HasId (Maybe OwnerId) env AssetId
    , HasSet FarthestLocationId env (InvestigatorId, LocationMatcher)
    , HasSet ClosestLocationId env (InvestigatorId, LocationMatcher)
    , HasSet EnemyId env EnemyMatcher
    , HasList UnderneathCard env InvestigatorId
    , HasList DeckCard env InvestigatorId
    , HasHistory env
    , Query AssetMatcher env
    , Query EnemyMatcher env
    , Query ExtendedCardMatcher env
    , Query LocationMatcher env
    , Query InvestigatorMatcher env
    , HasCount ActsRemainingCount env ()
    , HasCount DoomCount env ()
    , HasCount DoomCount env EnemyId
    , HasCount CardCount env InvestigatorId
    , HasCount ClueCount env InvestigatorId
    , HasCount HorrorCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount DoomCount env LocationId
    , HasCount DamageCount env InvestigatorId
    , HasCount PlayerCount env ()
    , HasCount ResourceCount env InvestigatorId
    , HasCount SetAsideCount env CardCode
    , HasCount Shroud env LocationId
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount TreacheryCount env (LocationId, CardCode)
    , HasId CardCode env AssetId
    , HasId CardCode env EnemyId
    , HasId LocationId env EnemyId
    , HasId LocationId env InvestigatorId
    , HasList UsedAbility env ()
    , HasList DiscardedPlayerCard env InvestigatorId
    , HasSet ActId env ()
    , HasSet ActId env TreacheryCardCode
    , HasSet AgendaId env ()
    , HasSet AgendaId env TreacheryCardCode
    , HasSet ClosestEnemyId env (LocationId, [Trait])
    , HasSet ClosestPathLocationId env (LocationId, LocationId)
    , HasSet ConnectedLocationId env LocationId
    , HasSet EnemyId env LocationId
    , HasSet EnemyId env Trait
    , HasSet EnemyId env CardCode
    , HasSet EnemyId env ([Trait], LocationId)
    , HasSet FarthestLocationId env InvestigatorId
    , HasSet HandCardId env (InvestigatorId, CardType)
    , HasSet InvestigatorId env LocationId
    , HasSet InvestigatorId env TreacheryCardCode
    , HasSet InvestigatorId env ()
    , HasSet LocationId env ()
    , HasSet LocationId env TreacheryCardCode
    , HasSet LocationId env [Trait]
    , HasSet ScenarioLogKey env ()
    , HasSet Trait env LocationId
    , HasSet FarthestEnemyId env (InvestigatorId, EnemyTrait)
    , HasSet EnemyId env ()
    )
