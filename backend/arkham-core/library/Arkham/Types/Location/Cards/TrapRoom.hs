module Arkham.Types.Location.Cards.TrapRoom
  ( trapRoom
  , TrapRoom(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing

newtype TrapRoom = TrapRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trapRoom :: LocationCard TrapRoom
trapRoom = location TrapRoom Cards.trapRoom 3 (PerPlayer 1) Moon [Diamond]

instance HasAbilities TrapRoom where
  getAbilities (TrapRoom attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ RevealLocation Timing.After You
      $ LocationWithId
      $ toId attrs
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env TrapRoom where
  runMessage msg l@(TrapRoom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      let
        getSomeRats =
          FindEncounterCard iid (toTarget attrs) $ cardIs Cards.swarmOfRats
      playerCount <- getPlayerCount
      l <$ pushAll (getSomeRats : [ getSomeRats | playerCount >= 3 ])
    FoundEncounterCard iid target card | isTarget attrs target ->
      l <$ push (SpawnEnemyAtEngagedWith (EncounterCard card) (toId attrs) iid)
    _ -> TrapRoom <$> runMessage msg attrs
