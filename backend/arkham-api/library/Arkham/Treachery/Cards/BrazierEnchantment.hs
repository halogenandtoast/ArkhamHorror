module Arkham.Treachery.Cards.BrazierEnchantment (brazierEnchantment) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Investigator (getSkillValue)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTarget, withSkillTest)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.UnionAndDisillusion.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Data.List.Extra (groupOn)

newtype BrazierEnchantment = BrazierEnchantment TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brazierEnchantment :: TreacheryCard BrazierEnchantment
brazierEnchantment = treachery BrazierEnchantment Cards.brazierEnchantment

instance HasModifiersFor BrazierEnchantment where
  getModifiersFor (BrazierEnchantment a) =
    runMaybeT_ do
      Action.Circle <- MaybeT getSkillTestAction
      LocationTarget {} <- MaybeT getSkillTestTarget
      lift $ withSkillTest \st -> modified_ a st [Difficulty 2]

instance HasAbilities BrazierEnchantment where
  getAbilities (BrazierEnchantment a) =
    [skillTestAbility $ mkAbility a 1 $ ActionAbility [Action.Circle] Nothing (ActionCost 1)]

instance RunMessage BrazierEnchantment where
  runMessage msg t@(BrazierEnchantment attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skills <- traverse (traverseToSnd (`getSkillValue` iid)) [minBound .. maxBound]

      let groups = groupOn snd $ sortOn snd skills
      let lowest = map fst $ concat $ take 1 groups
      let nextLowest = if length lowest > 1 then lowest else map fst $ concat $ take 1 $ drop 1 groups

      sid <- getRandom
      if length lowest == 2
        then circleTest sid iid (attrs.ability 1) iid lowest (Fixed 6)
        else do
          chooseOrRunOneM iid $ for_ lowest \skill1 -> do
            skillLabeled skill1 do
              chooseOrRunOneM iid $ for_ (deleteFirst skill1 nextLowest) \skill2 -> do
                skillLabeled skill2 do
                  circleTest sid iid (attrs.ability 1) iid [skill1, skill2] (Fixed 6)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> BrazierEnchantment <$> liftRunMessage msg attrs
