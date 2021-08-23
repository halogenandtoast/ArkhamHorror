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
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution

newtype GetTheEngineRunning = GetTheEngineRunning ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getTheEngineRunning :: ActCard GetTheEngineRunning
getTheEngineRunning =
  act (2, A) GetTheEngineRunning Cards.getTheEngineRunning Nothing

instance ActionRunner env => HasAbilities env GetTheEngineRunning where
  getAbilities i window (GetTheEngineRunning x) = do
    mEngineCar <- selectOne $ LocationWithTitle "Engine Car"
    case mEngineCar of
      Just engineCar -> do
        mustAdvance <- (== 0) . unClueCount <$> getCount engineCar
        if mustAdvance
          then pure [mkAbility x 1 LegacyForcedAbility]
          else getAbilities i window x
      Nothing -> getAbilities i window x

instance ActRunner env => RunMessage env GetTheEngineRunning where
  runMessage msg a@(GetTheEngineRunning attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    _ -> GetTheEngineRunning <$> runMessage msg attrs
