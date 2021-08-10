module Arkham.Types.Act.Cards.GetTheEngineRunning
  ( GetTheEngineRunning(..)
  , getTheEngineRunning
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Restriction

newtype GetTheEngineRunning = GetTheEngineRunning ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

getTheEngineRunning :: ActCard GetTheEngineRunning
getTheEngineRunning =
  act (2, A) GetTheEngineRunning Cards.getTheEngineRunning Nothing

instance HasActions GetTheEngineRunning where
  getActions (GetTheEngineRunning x) =
    [ restrictedAbility
        x
        1
        (LocationExists $ LocationWithTitle "Engine Car" <> LocationWithoutClues
        )
        (ForcedAbility AnyWindow)
    ]

instance ActRunner env => RunMessage env GetTheEngineRunning where
  runMessage msg a@(GetTheEngineRunning attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    _ -> GetTheEngineRunning <$> runMessage msg attrs
