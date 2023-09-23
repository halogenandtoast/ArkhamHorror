module Arkham.Act.Cards.AscendingTheHillV3 (
  AscendingTheHillV3 (..),
  ascendingTheHillV3,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype AscendingTheHillV3 = AscendingTheHillV3 ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV3 :: ActCard AscendingTheHillV3
ascendingTheHillV3 =
  act (2, A) AscendingTheHillV3 Cards.ascendingTheHillV3 Nothing

instance HasModifiersFor AscendingTheHillV3 where
  getModifiersFor (LocationTarget _) (AscendingTheHillV3 attrs) = do
    pure $ toModifiers attrs [TraitRestrictedModifier Altered CannotPlaceClues]
  getModifiersFor _ _ = pure []

instance HasAbilities AscendingTheHillV3 where
  getAbilities (AscendingTheHillV3 x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ Enters Timing.When You
        $ LocationWithTitle
          "Sentinel Peak"
    ]

instance RunMessage AscendingTheHillV3 where
  runMessage msg a@(AscendingTheHillV3 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) source AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      sentinelPeak <- selectJust $ LocationWithTitle "Sentinel Peak"
      sethBishop <- genCard Enemies.sethBishop
      createSethBishop <- createEnemyAt_ sethBishop sentinelPeak (Just $ toTarget attrs)
      pushAll
        [ createSethBishop
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    CreatedEnemyAt eid _ target | isTarget attrs target -> do
      damage <- perPlayer 1
      push $ PlaceDamage (toSource attrs) (toTarget eid) damage
      pure a
    _ -> AscendingTheHillV3 <$> runMessage msg attrs
