module Arkham.Enemy.Cards.GuardianOfTheCrystallizer (guardianOfTheCrystallizer) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype GuardianOfTheCrystallizer = GuardianOfTheCrystallizer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardianOfTheCrystallizer :: EnemyCard GuardianOfTheCrystallizer
guardianOfTheCrystallizer =
  enemyWith GuardianOfTheCrystallizer Cards.guardianOfTheCrystallizer (3, Static 3, 3) (1, 1)
    $ (exhaustedL .~ True)
    . (preyL .~ OnlyPrey (Prey $ HasMatchingAsset $ assetIs Assets.crystallizerOfDreams))

instance HasAbilities GuardianOfTheCrystallizer where
  getAbilities (GuardianOfTheCrystallizer x) =
    extend1 x
      $ groupLimit PerTestOrAbility
      $ restricted
        x
        1
        ( not_ (exists $ assetIs Assets.crystallizerOfDreams)
            <> not_ (exists $ EnemyWithPlacement Unplaced)
            <> NotInEliminatedBearersThreatArea
        )
      $ forced AnyWindow

instance RunMessage GuardianOfTheCrystallizer where
  runMessage msg e@(GuardianOfTheCrystallizer attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case enemyBearer attrs of
        Just bearer -> push $ PlaceInBonded bearer (toCard attrs)
        Nothing -> error "should not happen, this should have a bearer"
      pure e
    _ -> GuardianOfTheCrystallizer <$> runMessage msg attrs
