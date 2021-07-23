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
import Arkham.Types.Query
import Arkham.Types.Resolution

newtype GetTheEngineRunning = GetTheEngineRunning ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

getTheEngineRunning :: ActCard GetTheEngineRunning
getTheEngineRunning =
  act (2, A) GetTheEngineRunning Cards.getTheEngineRunning Nothing

instance ActionRunner env => HasActions env GetTheEngineRunning where
  getActions i window (GetTheEngineRunning x) = do
    mEngineCar <- getLocationIdWithTitle "Engine Car"
    case mEngineCar of
      Just engineCar -> do
        mustAdvance <- (== 0) . unClueCount <$> getCount engineCar
        if mustAdvance
          then pure [UseAbility i (mkAbility x 1 ForcedAbility)]
          else getActions i window x
      Nothing -> getActions i window x

instance ActRunner env => RunMessage env GetTheEngineRunning where
  runMessage msg a@(GetTheEngineRunning attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 1)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    _ -> GetTheEngineRunning <$> runMessage msg attrs
