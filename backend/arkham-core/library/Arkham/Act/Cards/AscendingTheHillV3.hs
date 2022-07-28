module Arkham.Act.Cards.AscendingTheHillV3
  ( AscendingTheHillV3(..)
  , ascendingTheHillV3
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype AscendingTheHillV3 = AscendingTheHillV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV3 :: ActCard AscendingTheHillV3
ascendingTheHillV3 =
  act (2, A) AscendingTheHillV3 Cards.ascendingTheHillV3 Nothing

instance HasModifiersFor AscendingTheHillV3 where
  getModifiersFor _ (LocationTarget _) (AscendingTheHillV3 attrs) = do
    pure $ toModifiers attrs [TraitRestrictedModifier Altered CannotPlaceClues]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AscendingTheHillV3 where
  getAbilities (AscendingTheHillV3 x) =
    [ mkAbility x 1 $ ForcedAbility $ Enters Timing.When You $ LocationWithTitle
        "Sentinel Peak"
    ]

instance RunMessage AscendingTheHillV3 where
  runMessage msg a@(AscendingTheHillV3 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      sentinelPeak <- fromJustNote "must exist"
        <$> selectOne (LocationWithTitle "Sentinel Peak")
      sethBishop <- EncounterCard <$> genEncounterCard Enemies.sethBishop
      a <$ pushAll
        [ CreateEnemyAt sethBishop sentinelPeak (Just $ toTarget attrs)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
    CreatedEnemyAt eid _ target | isTarget attrs target -> do
      damage <- getPlayerCountValue (PerPlayer 1)
      a <$ push (EnemySetDamage eid (toSource attrs) damage)
    _ -> AscendingTheHillV3 <$> runMessage msg attrs
