module Arkham.Location.Cards.TheEdgeOfTheUniverse
  ( theEdgeOfTheUniverse
  , TheEdgeOfTheUniverse(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( theEdgeOfTheUniverse )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Phase

newtype TheEdgeOfTheUniverse = TheEdgeOfTheUniverse LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEdgeOfTheUniverse :: LocationCard TheEdgeOfTheUniverse
theEdgeOfTheUniverse =
  location TheEdgeOfTheUniverse Cards.theEdgeOfTheUniverse 2 (PerPlayer 2)

instance HasModifiersFor TheEdgeOfTheUniverse where
  getModifiersFor (InvestigatorTarget iid) (TheEdgeOfTheUniverse attrs)
    | iid `on` attrs = do
      phase <- getPhase
      pure $ toModifiers attrs [ CannotDrawCards | phase == UpkeepPhase ]
  getModifiersFor _ _ = pure []

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

instance RunMessage TheEdgeOfTheUniverse where
  runMessage msg (TheEdgeOfTheUniverse attrs) =
    TheEdgeOfTheUniverse <$> runMessage msg attrs
