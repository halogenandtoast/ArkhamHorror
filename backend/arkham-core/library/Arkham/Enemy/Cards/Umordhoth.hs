module Arkham.Enemy.Cards.Umordhoth (
  Umordhoth (..),
  umordhoth,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype Umordhoth = Umordhoth EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhoth :: EnemyCard Umordhoth
umordhoth = enemy Umordhoth Cards.umordhoth (5, Static 6, 6) (3, 3)

instance HasModifiersFor Umordhoth where
  getModifiersFor target (Umordhoth a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 4)
    pure $ toModifiers a [HealthModifier healthModifier]
  getModifiersFor _ _ = pure []

instance HasAbilities Umordhoth where
  getAbilities (Umordhoth attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $ ForcedAbility $ TurnEnds Timing.After Anyone
      , withCriteria (mkAbility attrs 2 $ ActionAbility Nothing $ ActionCost 1)
          $ OnSameLocation
          <> AssetExists (AssetControlledBy You <> assetIs Cards.litaChantler)
      ]

instance RunMessage Umordhoth where
  runMessage msg e@(Umordhoth attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ Ready $ toTarget attrs
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push R3
      pure e
    _ -> Umordhoth <$> runMessage msg attrs
