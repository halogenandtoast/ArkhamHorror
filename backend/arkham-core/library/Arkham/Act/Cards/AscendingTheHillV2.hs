module Arkham.Act.Cards.AscendingTheHillV2
  ( AscendingTheHillV2(..)
  , ascendingTheHillV2
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype AscendingTheHillV2 = AscendingTheHillV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV2 :: ActCard AscendingTheHillV2
ascendingTheHillV2 =
  act (2, A) AscendingTheHillV2 Cards.ascendingTheHillV2 Nothing

instance HasModifiersFor AscendingTheHillV2 where
  getModifiersFor (LocationTarget _) (AscendingTheHillV2 attrs) =
    pure $ toModifiers attrs [TraitRestrictedModifier Altered CannotPlaceClues]
  getModifiersFor _ _ = pure []

instance HasAbilities AscendingTheHillV2 where
  getAbilities (AscendingTheHillV2 x) =
    [ mkAbility x 1 $ ForcedAbility $ Enters Timing.When You $ LocationWithTitle
        "Sentinel Peak"
    ]

instance RunMessage AscendingTheHillV2 where
  runMessage msg a@(AscendingTheHillV2 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      sentinelPeak <- fromJustNote "must exist"
        <$> selectOne (LocationWithTitle "Sentinel Peak")
      sethBishop <- EncounterCard <$> genEncounterCard Enemies.sethBishop
      a <$ pushAll
        [ CreateEnemyAt sethBishop sentinelPeak Nothing
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
    _ -> AscendingTheHillV2 <$> runMessage msg attrs
