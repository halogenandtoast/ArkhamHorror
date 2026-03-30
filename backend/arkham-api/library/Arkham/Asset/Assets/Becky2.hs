module Arkham.Asset.Assets.Becky2 (becky2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype Becky2 = Becky2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

becky2 :: AssetCard Becky2
becky2 = asset Becky2 Cards.becky2

instance HasAbilities Becky2 where
  getAbilities (Becky2 a) =
    [ skillTestAbility $ controlled_ a 1 $ fightAction (assetUseCost a Ammo 1)
    , controlled_ a 2 $ triggered (ActivateAbility #when You (AbilityIs (toSource a) 1)) (exhaust a)
    ]

instance RunMessage Becky2 where
  runMessage msg a@(Becky2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid source
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      thisSkillTestModifiers iid (attrs.ability 2) iid [IgnoreAloof, IgnoreRetaliate]
      pure a
    _ -> Becky2 <$> liftRunMessage msg attrs
