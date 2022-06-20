module Arkham.Location.Cards.MuseumEntrance
  ( museumEntrance
  , MuseumEntrance(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (museumEntrance)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Target

newtype MuseumEntrance = MuseumEntrance LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

museumEntrance :: LocationCard MuseumEntrance
museumEntrance =
  location MuseumEntrance Cards.museumEntrance 3 (Static 2) Circle [Square]

instance HasModifiersFor MuseumEntrance where
  getModifiersFor _ (InvestigatorTarget iid) (MuseumEntrance attrs) =
    pure $ toModifiers attrs [ CannotGainResources | iid `on` attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MuseumEntrance where
  getAbilities (MuseumEntrance a) = withResignAction a []

instance RunMessage MuseumEntrance where
  runMessage msg (MuseumEntrance attrs) =
    MuseumEntrance <$> runMessage msg attrs
