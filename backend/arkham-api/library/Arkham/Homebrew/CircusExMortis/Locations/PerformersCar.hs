module Arkham.Homebrew.CircusExMortis.Locations.PerformersCar (performersCar) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PerformersCar = PerformersCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

performersCar :: LocationCard PerformersCar
performersCar = location PerformersCar Cards.performersCar 2 (Static 1)

instance HasModifiersFor PerformersCar where
  getModifiersFor (PerformersCar a) = do
    n <- selectCount $ EnemyAt (be a)
    modifySelf a [ShroudModifier n | n > 0]

instance HasAbilities PerformersCar where
  getAbilities (PerformersCar a) = extendRevealed a []

instance RunMessage PerformersCar where
  runMessage msg (PerformersCar attrs) = runQueueT $ PerformersCar <$> liftRunMessage msg attrs
