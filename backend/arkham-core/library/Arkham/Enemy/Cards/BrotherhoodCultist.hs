module Arkham.Enemy.Cards.BrotherhoodCultist (
  brotherhoodCultist,
  BrotherhoodCultist (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyEvade, EnemyFight)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype BrotherhoodCultist = BrotherhoodCultist EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brotherhoodCultist :: EnemyCard BrotherhoodCultist
brotherhoodCultist =
  enemy BrotherhoodCultist Cards.brotherhoodCultist (2, Static 3, 2) (0, 1)

instance HasModifiersFor BrotherhoodCultist where
  getModifiersFor target (BrotherhoodCultist a) | isTarget a target = do
    doom <- field EnemyDoom (toId a)
    pure
      . toModifiers a
      $ if doom > 0
        then [EnemyFight doom, EnemyEvade doom]
        else []
  getModifiersFor _ _ = pure []

instance HasAbilities BrotherhoodCultist where
  getAbilities (BrotherhoodCultist a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ EnemyAttacked Timing.When You AnySource
          $ EnemyWithId
          $ toId a
      ]

instance RunMessage BrotherhoodCultist where
  runMessage msg e@(BrotherhoodCultist attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ PlaceTokens (toSource attrs) (toTarget attrs) Doom 1
      pure e
    _ -> BrotherhoodCultist <$> runMessage msg attrs
