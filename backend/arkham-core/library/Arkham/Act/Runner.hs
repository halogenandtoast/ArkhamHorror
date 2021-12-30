module Arkham.Act.Runner where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Decks
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.Query
import Arkham.ScenarioLogKey
import Arkham.Token
import Arkham.Trait (Trait)

type ActRunner env
  = ( HasQueue env
    , Query AssetMatcher env
    , Query EnemyMatcher env
    , Query InvestigatorMatcher env
    , Query LocationMatcher env
    , Query ExtendedCardMatcher env
    , HasCount ClueCount env AssetId
    , HasCount ClueCount env InvestigatorId
    , HasCount DamageCount env EnemyId
    , HasCount PlayerCount env ()
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount SpendableClueCount env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe OwnerId) env AssetId
    , HasId LocationId env EnemyId
    , HasId CardCode env AssetId
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasList Token env ()
    , HasList ResignedCardCode env ()
    , HasList UnderneathCard env ActDeck
    , HasList DiscardedEncounterCard env ()
    , HasRecord env ()
    , HasSet CompletedScenarioId env ()
    , HasSet EnemyId env LocationId
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env (HashSet LocationId)
    , HasSet InvestigatorId env LocationId
    , HasSet InvestigatorId env LocationMatcher
    , HasSet InScenarioInvestigatorId env ()
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet ScenarioLogKey env ()
    , HasSet Trait env AssetId
    , HasSet UnrevealedLocationId env ()
    , HasSet VictoryDisplayCardCode env ()
    , HasStep AgendaStep env ()
    , HasList LocationName env ()
    )
