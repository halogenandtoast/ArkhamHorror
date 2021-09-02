module Arkham.Types.Location.Cards.TheEdgeOfTheUniverse
  ( theEdgeOfTheUniverse
  , TheEdgeOfTheUniverse(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (theEdgeOfTheUniverse)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.Phase
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

instance HasAbilities TheEdgeOfTheUniverse where
  getAbilities (TheEdgeOfTheUniverse attrs) = do
    let actions = getAbilities attrs
    flip map actions $ \action -> case abilityType action of
      ActionAbility (Just Action.Move) _ -> action
        { abilityCriteria =
          Just $ fromMaybe mempty (abilityCriteria action) <> InvestigatorExists
            (You <> InvestigatorWithClues (AtLeast $ Static 2))
        }
      _ -> action

instance LocationRunner env => RunMessage env TheEdgeOfTheUniverse where
  runMessage msg (TheEdgeOfTheUniverse attrs) =
    TheEdgeOfTheUniverse <$> runMessage msg attrs
