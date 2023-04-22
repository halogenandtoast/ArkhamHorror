module Arkham.Enemy.Cards.Umordhoth
  ( Umordhoth(..)
  , umordhoth
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype Umordhoth = Umordhoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhoth :: EnemyCard Umordhoth
umordhoth = enemy Umordhoth Cards.umordhoth (5, Static 6, 6) (3, 3)

instance HasModifiersFor Umordhoth where
  getModifiersFor target (Umordhoth a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 4)
    pure $ toModifiers a [HealthModifier healthModifier]
  getModifiersFor _ _ = pure []

instance HasAbilities Umordhoth where
  getAbilities (Umordhoth attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ TurnEnds Timing.After Anyone
    , restrictedAbility
      attrs
      2
      (OnSameLocation
      <> AssetExists (AssetControlledBy You <> assetIs Cards.litaChantler)
      )
    $ ActionAbility Nothing
    $ ActionCost 1
    ]

instance RunMessage Umordhoth where
  runMessage msg e@(Umordhoth attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      e <$ push (Ready $ toTarget attrs)
    UseCardAbility _ source 2 _ _ | isSource attrs source ->
      e <$ push (ScenarioResolution $ Resolution 3)
    _ -> Umordhoth <$> runMessage msg attrs
