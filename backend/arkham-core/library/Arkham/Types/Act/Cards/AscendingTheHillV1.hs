module Arkham.Types.Act.Cards.AscendingTheHillV1
  ( AscendingTheHillV1(..)
  , ascendingTheHillV1
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype AscendingTheHillV1 = AscendingTheHillV1 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV1 :: ActCard AscendingTheHillV1
ascendingTheHillV1 =
  act (2, A) AscendingTheHillV1 Cards.ascendingTheHillV1 Nothing

instance HasActions AscendingTheHillV1 where
  getActions (AscendingTheHillV1 x) =
    mkAbility
        x
        1
        (Objective $ ForcedAbility $ Enters
          Timing.When
          Anyone
          (LocationWithTitle "Sentinel Peak")
        )
      : getActions x

instance HasSet Trait env LocationId => HasModifiersFor env AscendingTheHillV1 where
  getModifiersFor _ (LocationTarget lid) (AscendingTheHillV1 attrs) = do
    traits <- getSet lid
    pure $ toModifiers attrs [ CannotPlaceClues | Altered `notMember` traits ]
  getModifiersFor _ _ _ = pure []

instance ActRunner env => RunMessage env AscendingTheHillV1 where
  runMessage msg a@(AscendingTheHillV1 attrs@ActAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (NextAct actId "02281")
    _ -> AscendingTheHillV1 <$> runMessage msg attrs
