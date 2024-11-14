module Arkham.Location.Cards.NewChurchGreen (newChurchGreen, NewChurchGreen (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype NewChurchGreen = NewChurchGreen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newChurchGreen :: LocationCard NewChurchGreen
newChurchGreen = location NewChurchGreen Cards.newChurchGreen 3 (PerPlayer 2)

instance HasAbilities NewChurchGreen where
  getAbilities (NewChurchGreen a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        (Here <> ScenarioDeckWithCard LeadsDeck <> thisIs a LocationWithoutClues)
        doubleActionAbility

instance RunMessage NewChurchGreen where
  runMessage msg l@(NewChurchGreen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- take 1 <$> getLeadsDeck
      focusCards cards \unfocus -> continue iid [unfocus]
      for_ cards \card -> whenNothing card.victoryPoints (discard card)
      pure l
    _ -> NewChurchGreen <$> liftRunMessage msg attrs
