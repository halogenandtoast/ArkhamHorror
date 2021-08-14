module Arkham.Types.Location.Cards.TheEdgeOfTheUniverse
  ( theEdgeOfTheUniverse
  , TheEdgeOfTheUniverse(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (theEdgeOfTheUniverse)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Query
import Arkham.Types.Target

newtype TheEdgeOfTheUniverse = TheEdgeOfTheUniverse LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEdgeOfTheUniverse :: LocationCard TheEdgeOfTheUniverse
theEdgeOfTheUniverse = location
  TheEdgeOfTheUniverse
  Cards.theEdgeOfTheUniverse
  2
  (PerPlayer 2)
  Moon
  [Plus, Squiggle]

instance HasPhase env => HasModifiersFor env TheEdgeOfTheUniverse where
  getModifiersFor _ (InvestigatorTarget iid) (TheEdgeOfTheUniverse attrs)
    | iid `on` attrs = do
      phase <- getPhase
      pure $ toModifiers attrs [ CannotDrawCards | phase == UpkeepPhase ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasAbilities env TheEdgeOfTheUniverse where
  getAbilities iid window (TheEdgeOfTheUniverse attrs) = do
    actions <- getAbilities iid window attrs
    clueCount <- unClueCount <$> getCount iid
    pure $ if clueCount >= 2
      then actions
      else filter
        (\ability -> case abilityType ability of
          ActionAbility (Just Action.Move) _ -> False
          _ -> True
        )
        actions

instance LocationRunner env => RunMessage env TheEdgeOfTheUniverse where
  runMessage msg (TheEdgeOfTheUniverse attrs) =
    TheEdgeOfTheUniverse <$> runMessage msg attrs
