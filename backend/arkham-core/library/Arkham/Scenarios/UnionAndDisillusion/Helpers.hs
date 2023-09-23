module Arkham.Scenarios.UnionAndDisillusion.Helpers where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes.Entity
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

lightBrazier :: LocationId -> Message
lightBrazier locationId = UpdateLocation locationId (LocationBrazier ?=. Lit)

unlightBrazier :: LocationId -> Message
unlightBrazier locationId = UpdateLocation locationId (LocationBrazier ?=. Unlit)

circleTest
  :: (Sourceable source, Targetable target, HasQueue Message m)
  => InvestigatorId
  -> source
  -> target
  -> [SkillType]
  -> Int
  -> m ()
circleTest iid source target skillTypes n =
  push
    $ BeginSkillTest
    $ buildSkillTest
      iid
      source
      target
      (AndSkillTest skillTypes)
      (AndSkillBaseValue skillTypes)
      n

passedCircleTest :: HasQueue Message m => InvestigatorId -> LocationAttrs -> m ()
passedCircleTest iid attrs = do
  let
    brazierChoice =
      case locationBrazier attrs of
        Just Lit -> Label "Unlight the brazier" [unlightBrazier (toId attrs)]
        _unlit -> Label "Light the brazier" [lightBrazier (toId attrs)]
  push $ chooseOne iid [brazierChoice, Label "Leave brazier alone" []]

pattern DuringCircleAction :: Criterion
pattern DuringCircleAction <- DuringSkillTest (SkillTestForAction (ActionIs Circle))
  where
    DuringCircleAction = DuringSkillTest (SkillTestForAction (ActionIs Circle))
