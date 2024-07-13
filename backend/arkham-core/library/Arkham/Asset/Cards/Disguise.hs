module Arkham.Asset.Cards.Disguise (disguise, Disguise (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Phase

newtype Disguise = Disguise AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disguise :: AssetCard Disguise
disguise = asset Disguise Cards.disguise

instance HasAbilities Disguise where
  getAbilities (Disguise a) = [restrictedAbility a 1 ControlsThis $ evadeAction $ assetUseCost a Supply 1]

instance RunMessage Disguise where
  runMessage msg a@(Disguise attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (SkillModifier #agility 2)
      chooseEvadeEnemy iid (attrs.ability 1)
      pure a
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= traverse_ \case
        EnemyTarget eid -> do
          whenM (eid <=~> NonEliteEnemy) do
            nextPhaseModifier UpkeepPhase (attrs.ability 1) eid DoesNotReadyDuringUpkeep
        _ -> pure ()
      pure a
    _ -> Disguise <$> liftRunMessage msg attrs
