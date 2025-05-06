module Arkham.Location.Cards.MuseumEntrance (museumEntrance) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards (museumEntrance)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers

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
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "museumEntrance.resign" $ locationResignAction a

instance RunMessage MuseumEntrance where
  runMessage msg (MuseumEntrance attrs) = MuseumEntrance <$> runMessage msg attrs
