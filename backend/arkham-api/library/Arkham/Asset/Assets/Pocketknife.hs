module Arkham.Asset.Assets.Pocketknife (pocketknife) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype Pocketknife = Pocketknife AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pocketknife :: AssetCard Pocketknife
pocketknife = asset Pocketknife Cards.pocketknife

instance HasAbilities Pocketknife where
  getAbilities (Pocketknife a) =
    [ skillTestAbility
        $ controlled_ a 1
        $ ActionAbility #fight (Just $ OrAbilitySkills [#combat, #agility]) (ActionCost 1)
    ]

instance RunMessage Pocketknife where
  runMessage msg a@(Pocketknife attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      modifyAnySkill sid (attrs.ability 1) iid 1
      chooseFightEnemyWithSkillChoice sid iid (attrs.ability 1) [#combat, #agility]
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      for_ attrs.controller \iid -> do
        chooseOneM iid do
          (cardI18n $ labeled' "pocketknife.exhaustPocketknifeGain1Resource") do
            exhaustThis attrs
            gainResources iid (attrs.ability 1) 1
          labeledI "doNotExhaust" (pure ())
      pure a
    _ -> Pocketknife <$> liftRunMessage msg attrs
