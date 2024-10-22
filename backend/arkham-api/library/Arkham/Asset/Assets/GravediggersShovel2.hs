module Arkham.Asset.Assets.GravediggersShovel2 (gravediggersShovel2, GravediggersShovel2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype GravediggersShovel2 = GravediggersShovel2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gravediggersShovel2 :: AssetCard GravediggersShovel2
gravediggersShovel2 = asset GravediggersShovel2 Cards.gravediggersShovel2

instance HasAbilities GravediggersShovel2 where
  getAbilities (GravediggersShovel2 x) =
    [ fightAbility x 1 mempty ControlsThis
    , controlledAbility x 2 (youExist $ InvestigatorCanDiscoverCluesAt YourLocation)
        $ actionAbilityWithCost (OrCost [discardCost x, removeCost x])
    ]

instance RunMessage GravediggersShovel2 where
  runMessage msg a@(GravediggersShovel2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ (removedPayments -> removed) -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 2)
        $ if any (isTarget attrs) removed then 2 else 1
      pure a
    _ -> GravediggersShovel2 <$> liftRunMessage msg attrs
