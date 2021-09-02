module Arkham.Types.Enemy.Cards.Umordhoth
  ( Umordhoth(..)
  , umordhoth
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Resolution
import qualified Arkham.Types.Timing as Timing

newtype Umordhoth = Umordhoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhoth :: EnemyCard Umordhoth
umordhoth = enemy Umordhoth Cards.umordhoth (5, Static 6, 6) (3, 3)

instance HasCount PlayerCount env () => HasModifiersFor env Umordhoth where
  getModifiersFor _ target (Umordhoth a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 4)
    pure $ toModifiers a [HealthModifier healthModifier]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Umordhoth where
  getAbilities (Umordhoth attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ TurnEnds Timing.After Anyone
    , restrictedAbility
      attrs
      2
      (OnSameLocation
      <> AssetExists (AssetOwnedBy You <> assetIs Cards.litaChantler)
      )
    $ ActionAbility Nothing
    $ ActionCost 1
    ]

instance EnemyRunner env => RunMessage env Umordhoth where
  runMessage msg e@(Umordhoth attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (Ready $ toTarget attrs)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      e <$ push (ScenarioResolution $ Resolution 3)
    _ -> Umordhoth <$> runMessage msg attrs
