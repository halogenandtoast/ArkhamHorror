module Arkham.Skill.Cards.WatchThis (
  watchThis,
  WatchThis (..),
) where

import Arkham.Calculation
import Arkham.Classes
import Arkham.Cost
import Arkham.Message
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype WatchThis = WatchThis SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchThis :: SkillCard WatchThis
watchThis =
  skillWith
    WatchThis
    Cards.watchThis
    (additionalCostL ?~ UpTo (Fixed 3) (ResourceCost 1))

paymentAmount :: Payment -> Int
paymentAmount (Payments xs) = sum $ map paymentAmount xs
paymentAmount (ResourcePayment n) = n
paymentAmount _ = 0

instance RunMessage WatchThis where
  runMessage msg s@(WatchThis attrs) = case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 1 -> do
      let resources = 2 * maybe 0 paymentAmount (skillAdditionalPayment attrs)
      push $ TakeResources iid resources (toSource attrs) False
      pure s
    _ -> WatchThis <$> runMessage msg attrs
