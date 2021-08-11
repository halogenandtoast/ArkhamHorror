module Arkham.Types.Act.Cards.AscendingTheHillV3
  ( AscendingTheHillV3(..)
  , ascendingTheHillV3
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
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype AscendingTheHillV3 = AscendingTheHillV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV3 :: ActCard AscendingTheHillV3
ascendingTheHillV3 =
  act (2, A) AscendingTheHillV3 Cards.ascendingTheHillV3 Nothing

instance HasActions AscendingTheHillV3 where
  getActions (AscendingTheHillV3 x) =
    mkAbility
        x
        1
        (Objective $ ForcedAbility $ Enters
          Timing.When
          Anyone
          (LocationWithTitle "Sentinel Peak")
        )
      : getActions x

instance HasSet Trait env LocationId => HasModifiersFor env AscendingTheHillV3 where
  getModifiersFor _ (LocationTarget lid) (AscendingTheHillV3 attrs) = do
    traits <- getSet lid
    pure $ toModifiers attrs [ CannotPlaceClues | Altered `notMember` traits ]
  getModifiersFor _ _ _ = pure []

instance ActRunner env => RunMessage env AscendingTheHillV3 where
  runMessage msg a@(AscendingTheHillV3 attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      sentinelPeak <- fromJustNote "must exist"
        <$> selectOne (LocationWithTitle "Sentinel Peak")
      sethBishop <- EncounterCard <$> genEncounterCard Enemies.sethBishop
      a <$ pushAll
        [ CreateEnemyAt sethBishop sentinelPeak (Just $ toTarget attrs)
        , NextAct actId "02281"
        ]
    CreatedEnemyAt eid _ target | isTarget attrs target -> do
      damage <- getPlayerCountValue (PerPlayer 1)
      a <$ push (EnemySetDamage eid (toSource attrs) damage)
    _ -> AscendingTheHillV3 <$> runMessage msg attrs
