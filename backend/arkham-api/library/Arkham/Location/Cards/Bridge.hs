module Arkham.Location.Cards.Bridge (bridge) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Trait (Trait (Manifold))

newtype Bridge = Bridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bridge :: LocationCard Bridge
bridge = locationWith Bridge Cards.bridge 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Bridge where
  getAbilities (Bridge a) =
    let usedSuccess = toResultDefault False a.meta
     in extendRevealed a
          $ [ scenarioI18n
              $ withI18nTooltip "bridge.defeat"
              $ skillTestAbility
              $ restricted a 1 (exists (exhaustedManifoldAt a))
              $ FastAbility Free
            | not usedSuccess
            ]
   where
    exhaustedManifoldAt loc = EnemyAt (be loc) <> ExhaustedEnemy <> EnemyWithTrait Manifold

instance RunMessage Bridge where
  runMessage msg l@(Bridge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (be attrs) <> ExhaustedEnemy <> EnemyWithTrait Manifold
      sid <- getRandom
      chooseTargetM iid enemies \enemy ->
        beginSkillTest sid iid (attrs.ability 1) enemy #intellect (EnemyMaybeFieldCalculation enemy EnemyEvade)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ \eid -> defeatEnemy eid iid (attrs.ability 1)
      pure $ Bridge $ setMeta True attrs
    _ -> Bridge <$> liftRunMessage msg attrs
