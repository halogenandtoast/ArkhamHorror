module Arkham.Asset.Cards.Pitchfork (pitchfork, Pitchfork (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Modifier
import Arkham.Placement

newtype Pitchfork = Pitchfork AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pitchfork :: AssetCard Pitchfork
pitchfork = asset Pitchfork Cards.pitchfork

instance HasAbilities Pitchfork where
  getAbilities (Pitchfork a) =
    restrictedAbility a 1 ControlsThis fightAction_
      : case a.placement of
        AttachedToLocation lid ->
          [ withTooltip "Take control of Pitchfork" $ restrictedAbility (proxied lid a) 2 Here actionAbility
          ]
        _ -> []
   where

instance RunMessage Pitchfork where
  runMessage msg a@(Pitchfork attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 1, DamageDealt 2]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withLocationOf iid \lid -> do
        push $ PlaceAsset attrs.id $ AttachedToLocation lid
        push $ LoseControlOfAsset attrs.id
      pure a
    UseThisAbility iid (isProxySource attrs -> True) 2 -> do
      push $ TakeControlOfAsset iid attrs.id
      pure a
    _ -> Pitchfork <$> liftRunMessage msg attrs
