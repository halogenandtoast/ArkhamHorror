module Arkham.Scenarios.UnionAndDisillusion.Helpers where

import Arkham.Prelude

import Arkham.Message.Lifted.Queue
import Arkham.Message.Lifted.Choose
import Arkham.Action
import Arkham.Calculation
import Arkham.Classes.HasQueue
import Arkham.Criteria
import Arkham.Field
import Arkham.Id
import Arkham.Location.Brazier
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

lightBrazier :: ReverseQueue m => LocationId -> m ()
lightBrazier locationId = push $ UpdateLocation locationId (LocationBrazier ?=. Lit)

unlightBrazier :: ReverseQueue m => LocationId -> m ()
unlightBrazier locationId = push $ UpdateLocation locationId (LocationBrazier ?=. Unlit)

circleTest
  :: (Sourceable source, Targetable target, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> [SkillType]
  -> GameCalculation
  -> m ()
circleTest sid iid source target skillTypes n =
  push
    $ BeginSkillTest
    $ ( buildSkillTest
          sid
          iid
          source
          target
          (AndSkillTest skillTypes)
          (AndSkillBaseValue skillTypes)
          (SkillTestDifficulty n)
      )
      { skillTestAction = Just Circle
      }

passedCircleTest :: ReverseQueue m => InvestigatorId -> LocationAttrs -> m ()
passedCircleTest iid attrs = chooseOneM iid do
  case locationBrazier attrs of
    Just Lit -> labeled "Unlight the brazier" $ unlightBrazier attrs.id
    _unlit -> labeled "Light the brazier" $ lightBrazier attrs.id
  labeled "Leave brazier alone" nothing

pattern DuringCircleAction :: Criterion
pattern DuringCircleAction <- DuringSkillTest (SkillTestWithAction (ActionIs Circle))
  where
    DuringCircleAction = DuringSkillTest (SkillTestWithAction (ActionIs Circle))
