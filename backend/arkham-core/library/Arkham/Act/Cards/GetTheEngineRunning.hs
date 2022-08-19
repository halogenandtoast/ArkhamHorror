module Arkham.Act.Cards.GetTheEngineRunning
  ( GetTheEngineRunning(..)
  , getTheEngineRunning
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution

newtype GetTheEngineRunning = GetTheEngineRunning ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getTheEngineRunning :: ActCard GetTheEngineRunning
getTheEngineRunning =
  act (2, A) GetTheEngineRunning Cards.getTheEngineRunning Nothing

instance HasAbilities GetTheEngineRunning where
  getAbilities (GetTheEngineRunning x) | onSide A x =
    [ restrictedAbility
        x
        1
        (LocationExists $ LocationWithTitle "Engine Car" <> LocationWithoutClues
        )
      $ Objective
      $ ForcedAbility AnyWindow
    ]
  getAbilities _ = []

instance RunMessage GetTheEngineRunning where
  runMessage msg a@(GetTheEngineRunning attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    _ -> GetTheEngineRunning <$> runMessage msg attrs
