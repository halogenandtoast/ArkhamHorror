module Arkham.Act.Cards.AtTheStationTrainTracks
  ( AtTheStationTrainTracks(..)
  , atTheStationTrainTracks
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype AtTheStationTrainTracks = AtTheStationTrainTracks ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheStationTrainTracks :: ActCard AtTheStationTrainTracks
atTheStationTrainTracks =
  act (2, C) AtTheStationTrainTracks Cards.atTheStationTrainTracks Nothing

instance RunMessage AtTheStationTrainTracks where
  runMessage msg (AtTheStationTrainTracks attrs) =
    AtTheStationTrainTracks <$> runMessage msg attrs
