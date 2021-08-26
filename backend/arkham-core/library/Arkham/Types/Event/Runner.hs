module Arkham.Types.Event.Runner where

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Trait

type EventRunner env
  = ( HasQueue env
    , Query AssetMatcher env
    , Query LocationMatcher env
    , HasCount ClueCount env InvestigatorId
    , HasCount ClueCount env LocationId
    , HasCount PlayerCount env ()
    , HasCount UsesCount env AssetId
    , HasId LocationId env InvestigatorId
    , HasList DiscardableHandCard env InvestigatorId
    , HasSet AccessibleLocationId env LocationId
    , HasSet ConnectedLocationId env LocationId
    , HasSet EmptyLocationId env ()
    , HasSet EnemyId env InvestigatorId
    , HasSet EnemyId env LocationId
    , HasSet ExhaustedEnemyId env LocationId
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env LocationId
    , HasSet RevealedLocationId env ()
    , HasSet Trait env AssetId
    , HasSet Trait env EnemyId
    , HasSkillTest env
    )
