module Arkham.Types.Skill.Runner where

import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher

type SkillRunner env
  = ( HasQueue env
    , Query ExtendedCardMatcher env
    , HasSet ConnectedLocationId env LocationId
    , HasSet BlockedLocationId env ()
    , HasSet EnemyId env InvestigatorId
    , HasId LocationId env InvestigatorId
    )
