module Arkham.Types.Skill.Runner where

import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Source
import Arkham.Types.Trait

type SkillRunner env
  = ( HasQueue env
    , Query ExtendedCardMatcher env
    , HasSet ConnectedLocationId env LocationId
    , HasSet BlockedLocationId env ()
    , HasSet EnemyId env InvestigatorId
    , HasSet Trait env Source
    , HasId LocationId env InvestigatorId
    , HasModifiersFor env ()
    , HasSkillTest env
    , HasCount DamageCount env InvestigatorId
    )
