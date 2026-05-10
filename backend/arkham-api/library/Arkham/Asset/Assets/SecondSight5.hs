module Arkham.Asset.Assets.SecondSight5 (secondSight5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message (setOptionCriteria)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype SecondSight5 = SecondSight5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondSight5 :: AssetCard SecondSight5
secondSight5 = asset SecondSight5 Cards.secondSight5

instance HasAbilities SecondSight5 where
  getAbilities (SecondSight5 a) = [skillTestAbility $ controlled_ a 1 $ investigateActionWith_ #willpower]

instance RunMessage SecondSight5 where
  runMessage msg a@(SecondSight5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #cultist attrs attrs $ doStep 1 msg
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
      investigateWith_ #willpower sid iid (attrs.ability 1)
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      if attrs.use #charge == 0
        then do
          assignHorror iid (attrs.ability 1) 1
          toDiscardBy iid (attrs.ability 1) attrs
        else removeTokens (attrs.ability 1) attrs Charge 1
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      additionalSkillTestOptionEdit
        ( setOptionCriteria
            $ exists (orConnected_ (locationWithInvestigator iid) <> locationWithDiscoverableCluesBy iid)
            <> thisExists attrs (AssetWithSpendableUses (atLeast 1) Charge)
        )
        "Second Sight (5)"
        do
          chooseOneM iid do
            (cardI18n $ labeled' "secondSight5.spend1ChargeToDiscover1AdditionalClue") $ doStep 2 msg
            (cardI18n $ labeled' "secondSight5.doNotSpendCharge") nothing
      pure a
    DoStep 2 (PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> do
      spendUses (attrs.ability 1) attrs Charge 1
      discoverAtMatchingLocation_ iid (attrs.ability 1) (orConnected_ $ locationWithInvestigator iid) 1
      pure a
    _ -> SecondSight5 <$> liftRunMessage msg attrs
