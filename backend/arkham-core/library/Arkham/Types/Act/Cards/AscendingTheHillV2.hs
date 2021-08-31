module Arkham.Types.Act.Cards.AscendingTheHillV2
  ( AscendingTheHillV2(..)
  , ascendingTheHillV2
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype AscendingTheHillV2 = AscendingTheHillV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV2 :: ActCard AscendingTheHillV2
ascendingTheHillV2 =
  act (2, A) AscendingTheHillV2 Cards.ascendingTheHillV2 Nothing

instance HasSet Trait env LocationId => HasModifiersFor env AscendingTheHillV2 where
  getModifiersFor _ (LocationTarget lid) (AscendingTheHillV2 attrs) = do
    traits <- getSet lid
    pure $ toModifiers attrs [ CannotPlaceClues | Altered `notMember` traits ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env AscendingTheHillV2 where
  getAbilities _ _ (AscendingTheHillV2 x) = pure
    [ mkAbility x 1 $ ForcedAbility $ Enters Timing.When You $ LocationWithTitle
        "Sentinel Peak"
    ]

instance ActRunner env => RunMessage env AscendingTheHillV2 where
  runMessage msg a@(AscendingTheHillV2 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      sentinelPeak <- fromJustNote "must exist"
        <$> selectOne (LocationWithTitle "Sentinel Peak")
      sethBishop <- EncounterCard <$> genEncounterCard Enemies.sethBishop
      a <$ pushAll
        [ CreateEnemyAt sethBishop sentinelPeak Nothing
        , NextAct (toId attrs) "02281"
        ]
    _ -> AscendingTheHillV2 <$> runMessage msg attrs
