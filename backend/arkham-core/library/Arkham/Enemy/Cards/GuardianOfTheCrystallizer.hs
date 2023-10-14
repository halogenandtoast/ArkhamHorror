module Arkham.Enemy.Cards.GuardianOfTheCrystallizer (
  guardianOfTheCrystallizer,
  GuardianOfTheCrystallizer (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Placement

newtype GuardianOfTheCrystallizer = GuardianOfTheCrystallizer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardianOfTheCrystallizer :: EnemyCard GuardianOfTheCrystallizer
guardianOfTheCrystallizer =
  enemyWith
    GuardianOfTheCrystallizer
    Cards.guardianOfTheCrystallizer
    (3, Static 3, 3)
    (1, 1)
    $ (exhaustedL .~ True)
    . (preyL .~ OnlyPrey (HasMatchingAsset $ assetIs Assets.crystallizerOfDreams))

instance HasAbilities GuardianOfTheCrystallizer where
  getAbilities (GuardianOfTheCrystallizer x) =
    withBaseAbilities
      x
      [ groupLimit PerTestOrAbility
          $ restrictedAbility
            x
            1
            ( Negate (exists $ assetIs Assets.crystallizerOfDreams)
                <> Negate (exists $ EnemyWithPlacement Unplaced)
            )
          $ ForcedAbility AnyWindow
      ]

instance RunMessage GuardianOfTheCrystallizer where
  runMessage msg e@(GuardianOfTheCrystallizer attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case enemyBearer attrs of
        Just bearer -> push $ PlaceInBonded bearer (toCard attrs)
        Nothing -> error "should not happen, this should have a bearer"
      pure e
    _ -> GuardianOfTheCrystallizer <$> runMessage msg attrs
