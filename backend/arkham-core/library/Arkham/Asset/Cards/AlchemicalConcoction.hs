module Arkham.Asset.Cards.AlchemicalConcoction (alchemicalConcoction, AlchemicalConcoction (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype AlchemicalConcoction = AlchemicalConcoction AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalConcoction :: AssetCard AlchemicalConcoction
alchemicalConcoction = asset AlchemicalConcoction Cards.alchemicalConcoction

instance HasAbilities AlchemicalConcoction where
  getAbilities (AlchemicalConcoction a) = [fightAbility a 1 mempty ControlsThis]

instance HasModifiersFor AlchemicalConcoction where
  getModifiersFor (InvestigatorTarget _) (AlchemicalConcoction a) = do
    toModifiers a . maybeToList <$> runMaybeT do
      Action.Fight <- MaybeT getSkillTestAction
      guardM $ isAbilitySource a 1 <$> MaybeT getSkillTestSource
      EnemyTarget eid <- MaybeT getSkillTestTarget
      isTheExperiment <- lift $ enemyMatches eid $ EnemyWithTitle "The Experiment"
      guard isTheExperiment
      pure $ DamageDealt 6
  getModifiersFor _ _ = pure []

instance RunMessage AlchemicalConcoction where
  runMessage msg a@(AlchemicalConcoction attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = attrs.ability 1
      chooseFight <- aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight iid source)
      pushAll $ skillTestModifier source iid (DamageDealt 1) : leftOr chooseFight
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ RemoveFromGame (toTarget attrs)
      pure a
    _ -> AlchemicalConcoction <$> runMessage msg attrs
