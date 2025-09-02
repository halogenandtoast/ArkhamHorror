module Arkham.Location.Cards.MoonlitGardenPoisonedBeauty (moonlitGardenPoisonedBeauty) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Window (evadedEnemy)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.FilmFatale.Helpers

newtype MoonlitGardenPoisonedBeauty = MoonlitGardenPoisonedBeauty LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitGardenPoisonedBeauty :: LocationCard MoonlitGardenPoisonedBeauty
moonlitGardenPoisonedBeauty = location MoonlitGardenPoisonedBeauty Cards.moonlitGardenPoisonedBeauty 3 (PerPlayer 1)

instance HasAbilities MoonlitGardenPoisonedBeauty where
  getAbilities (MoonlitGardenPoisonedBeauty a) =
    extend a
      $ if a.revealed
        then
          [ scenarioI18n $ hauntedI "moonlitGarden.haunted" a 2
          , groupLimit PerRound
              $ restricted a 3 Here
              $ freeReaction
              $ SkillTestResult #after You (WhileEvadingAnEnemy NonEliteEnemy) (SuccessResult $ atLeast 2)
          ]
        else
          [ restricted a 1 (exists $ enemyIs Enemies.theContessaNeedlesslySmug <> EnemyCanMove)
              $ forced
              $ UnrevealedRevealLocation #when You (be a)
          ]

instance RunMessage MoonlitGardenPoisonedBeauty where
  runMessage msg l@(MoonlitGardenPoisonedBeauty attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      moveContessa (attrs.ability 1) attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ NearestEnemyTo iid EnemyWithAnyDamage
      if null enemies
        then moveContessa (attrs.ability 2) attrs
        else chooseTargetM iid enemies $ healDamageOn (attrs.ability 2) 2
      pure l
    UseCardAbility iid (isSource attrs -> True) 3 (evadedEnemy -> enemy) _ -> do
      locations <- select $ connectedTo (locationWithInvestigator iid)
      chooseTargetM iid locations $ enemyMoveTo (attrs.ability 3) enemy
      pure l
    _ -> MoonlitGardenPoisonedBeauty <$> liftRunMessage msg attrs
