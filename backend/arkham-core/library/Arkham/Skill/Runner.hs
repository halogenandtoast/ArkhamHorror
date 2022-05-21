module Arkham.Skill.Runner where

import Arkham.Asset.Attrs
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.Query
import Arkham.SkillTest
import Arkham.Source
import Arkham.Trait

type SkillRunner env
  = ( HasQueue env
    , Query ExtendedCardMatcher env
    , Query AssetMatcher env
    , Projection env AssetAttrs
    , HasSet ConnectedLocationId env LocationId
    , HasSet BlockedLocationId env ()
    , HasSet EnemyId env InvestigatorId
    , HasSet Trait env Source
    , HasId LocationId env InvestigatorId
    , HasModifiersFor env ()
    , HasSkillTest env
    , HasCount DamageCount env InvestigatorId
    , Query InvestigatorMatcher env
    , Query EnemyMatcher env
    )
