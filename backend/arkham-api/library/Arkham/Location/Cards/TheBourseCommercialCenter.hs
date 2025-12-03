module Arkham.Location.Cards.TheBourseCommercialCenter (theBourseCommercialCenter) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.DogsOfWar.Helpers

newtype TheBourseCommercialCenter = TheBourseCommercialCenter LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBourseCommercialCenter :: LocationCard TheBourseCommercialCenter
theBourseCommercialCenter = symbolLabel $ location TheBourseCommercialCenter Cards.theBourseCommercialCenter 4 (PerPlayer 1)

instance HasAbilities TheBourseCommercialCenter where
  getAbilities (TheBourseCommercialCenter a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "theBourse.investigate"
      $ groupLimit PerRound
      $ restricted a 1 Here investigateAction_

instance RunMessage TheBourseCommercialCenter where
  runMessage msg l@(TheBourseCommercialCenter attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid $ scenarioI18n do
        labeled' "theBourse.reduceDifficulty" do
          skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
          doStep 1 msg
        labeled' "theBourse.doNotReduce" nothing
      investigate_ sid iid (attrs.ability 1)
      pure $ TheBourseCommercialCenter $ attrs & setMeta False
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      pure $ TheBourseCommercialCenter $ attrs & setMeta True
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      let reduced = toResultDefault False attrs.meta
      gainResources iid (attrs.ability 1) (if reduced then 2 else 4)
      pure l
    _ -> TheBourseCommercialCenter <$> liftRunMessage msg attrs
