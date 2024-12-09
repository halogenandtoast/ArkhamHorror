module Arkham.Location.Cards.MuseumEntrance (museumEntrance, MuseumEntrance (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (museumEntrance)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MuseumEntrance = MuseumEntrance LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

museumEntrance :: LocationCard MuseumEntrance
museumEntrance = location MuseumEntrance Cards.museumEntrance 3 (Static 2)

instance HasModifiersFor MuseumEntrance where
  getModifiersFor (MuseumEntrance a) =
    whenRevealed a $ modifySelect a (investigatorAt a) [CannotGainResources]

instance HasAbilities MuseumEntrance where
  getAbilities (MuseumEntrance a) =
    extendRevealed1 a
      $ withTooltip "\"Eh, How important can a book really be, anyway?\""
      $ locationResignAction a

instance RunMessage MuseumEntrance where
  runMessage msg (MuseumEntrance attrs) = MuseumEntrance <$> runMessage msg attrs
