module Arkham.Enemy.Runner where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Direction
import Arkham.Id
import Arkham.Matcher
import Arkham.Prey
import Arkham.Query
import Arkham.SkillTest
import Arkham.Source
import Arkham.Trait

type EnemyRunner env
  = ( HasQueue env
    , HasName env AssetId
    , HasSet EnemyId env ()
    , Query LocationMatcher env
    , Query EnemyMatcher env
    , Query InvestigatorMatcher env
    , HasCount CardCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount PlayerCount env ()
    , HasCount RemainingSanity env InvestigatorId
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId (Maybe LocationId) env (Direction, LocationId)
    , HasId LeadInvestigatorId env ()
    , HasId LocationId env InvestigatorId
    , HasModifiersFor env ()
    , HasSet ActId env ()
    , HasSet
        ClosestPathLocationId
        env
        (LocationId, LocationId, HashMap LocationId [LocationId])
    , HasSet
        ClosestPathLocationId
        env
        (LocationId, Prey, HashMap LocationId [LocationId])
    , HasSet ConnectedLocationId env LocationId
    , HasSet ClosestPathLocationId env (LocationId, Prey)
    , HasSet EmptyLocationId env ()
    , HasSet FarthestLocationId env InvestigatorId
    , HasSet FarthestLocationId env [InvestigatorId]
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env LocationId
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet PreyId env (Prey, LocationId)
    , HasSet PreyId env Prey
    , HasSet Trait env EnemyId
    , HasSet Trait env InvestigatorId
    , HasSet Trait env SkillId
    , HasSet Trait env AssetId
    , HasSet Trait env EventId
    , HasSet Trait env LocationId
    , HasSet Trait env Source
    , HasSkillTest env
    , HasStep AgendaStep env ()
    , HasId (Maybe OwnerId) env AssetId
    , HasId OwnerId env EventId
    , HasId OwnerId env SkillId
    )
