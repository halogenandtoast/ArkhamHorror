module Arkham.Types.Act.Cards.AscendingTheHillV2
  ( AscendingTheHillV2(..)
  , ascendingTheHillV2
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.ActId
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Trait

newtype AscendingTheHillV2 = AscendingTheHillV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions)

ascendingTheHillV2 :: ActCard AscendingTheHillV2
ascendingTheHillV2 =
  act (2, A) AscendingTheHillV2 Cards.ascendingTheHillV2 Nothing

instance HasSet Trait env LocationId => HasModifiersFor env AscendingTheHillV2 where
  getModifiersFor _ (LocationTarget lid) (AscendingTheHillV2 attrs) = do
    traits <- getSet lid
    pure $ toModifiers attrs [ CannotPlaceClues | Altered `notMember` traits ]
  getModifiersFor _ _ _ = pure []

instance (HasName env LocationId, ActRunner env) => RunMessage env AscendingTheHillV2 where
  runMessage msg a@(AscendingTheHillV2 attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)]
      pure
        . AscendingTheHillV2
        $ attrs
        & (sequenceL .~ Act (unActStep $ actStep actSequence) B)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      sentinelPeak <- fromJustNote "must exist"
        <$> getLocationIdWithTitle "Sentinel Peak"
      sethBishop <- EncounterCard <$> genEncounterCard Enemies.sethBishop
      a <$ pushAll
        [CreateEnemyAt sethBishop sentinelPeak Nothing, NextAct actId "02281"]
    WhenEnterLocation _ lid -> do
      name <- getName lid
      a <$ when
        (name == "Sentinel Peak")
        (push $ AdvanceAct actId (toSource attrs))
    _ -> AscendingTheHillV2 <$> runMessage msg attrs
