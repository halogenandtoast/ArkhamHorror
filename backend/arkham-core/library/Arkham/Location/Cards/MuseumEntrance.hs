module Arkham.Location.Cards.MuseumEntrance (
  museumEntrance,
  MuseumEntrance (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (museumEntrance)
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype MuseumEntrance = MuseumEntrance LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

museumEntrance :: LocationCard MuseumEntrance
museumEntrance = location MuseumEntrance Cards.museumEntrance 3 (Static 2)

instance HasModifiersFor MuseumEntrance where
  getModifiersFor (InvestigatorTarget iid) (MuseumEntrance attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs [CannotGainResources | here]
  getModifiersFor _ _ = pure []

instance HasAbilities MuseumEntrance where
  getAbilities (MuseumEntrance a) =
    withRevealedAbilities a
      $ [withTooltip "\"Eh, How important can a book really be, anyway?\"" $ locationResignAction a]

instance RunMessage MuseumEntrance where
  runMessage msg (MuseumEntrance attrs) =
    MuseumEntrance <$> runMessage msg attrs
