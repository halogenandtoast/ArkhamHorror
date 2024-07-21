module Arkham.Scenarios.UnionAndDisillusion.Helpers where

import Arkham.Prelude

import Arkham.Action
import Arkham.Calculation
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Criteria
import Arkham.Field
import Arkham.Helpers.Query
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

lightBrazier :: LocationId -> Message
lightBrazier locationId = UpdateLocation locationId (LocationBrazier ?=. Lit)

unlightBrazier :: LocationId -> Message
unlightBrazier locationId = UpdateLocation locationId (LocationBrazier ?=. Unlit)

circleTest
  :: (Sourceable source, Targetable target, HasQueue Message m)
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
    $ buildSkillTest
      sid
      iid
      source
      target
      (AndSkillTest skillTypes)
      (AndSkillBaseValue skillTypes)
      (SkillTestDifficulty n)

passedCircleTest :: (HasGame m, HasQueue Message m) => InvestigatorId -> LocationAttrs -> m ()
passedCircleTest iid attrs = do
  let
    brazierChoice =
      case locationBrazier attrs of
        Just Lit -> Label "Unlight the brazier" [unlightBrazier (toId attrs)]
        _unlit -> Label "Light the brazier" [lightBrazier (toId attrs)]
  player <- getPlayer iid
  push $ chooseOne player [brazierChoice, Label "Leave brazier alone" []]

pattern DuringCircleAction :: Criterion
pattern DuringCircleAction <- DuringSkillTest (SkillTestWithAction (ActionIs Circle))
  where
    DuringCircleAction = DuringSkillTest (SkillTestWithAction (ActionIs Circle))
