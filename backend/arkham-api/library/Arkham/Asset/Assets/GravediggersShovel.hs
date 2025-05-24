module Arkham.Asset.Assets.GravediggersShovel (gravediggersShovel) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype GravediggersShovel = GravediggersShovel AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gravediggersShovel :: AssetCard GravediggersShovel
gravediggersShovel = asset GravediggersShovel Cards.gravediggersShovel

instance HasAbilities GravediggersShovel where
  getAbilities (GravediggersShovel x) =
    [ fightAbility x 1 mempty ControlsThis
    , controlled x 2 (youExist $ InvestigatorCanDiscoverCluesAt YourLocation)
        $ actionAbilityWithCost (discardCost x)
    ]

instance RunMessage GravediggersShovel where
  runMessage msg a@(GravediggersShovel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 2) 1
      pure a
    _ -> GravediggersShovel <$> liftRunMessage msg attrs
