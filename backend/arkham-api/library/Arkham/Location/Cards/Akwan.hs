module Arkham.Location.Cards.Akwan (akwan) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (pattern IsDay1, pattern IsDay2)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheLostSister.Helpers
import Arkham.Strategy

newtype Akwan = Akwan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

akwan :: LocationCard Akwan
akwan = locationWith Akwan Cards.akwan 3 (Static 0) connectsToAdjacent

instance HasAbilities Akwan where
  getAbilities (Akwan a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "akwan.resign" $ locationResignAction a
      , restricted a 2 (ScenarioDeckWithCard CavernsDeck <> oneOf [IsDay1, IsDay2]) parleyAction_
      ]

instance RunMessage Akwan where
  runMessage msg l@(Akwan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      lookAt iid (attrs.ability 2) (ScenarioDeckTarget CavernsDeck) [(FromTopOfDeck 3, PutBackInAnyOrder)] #any ReturnCards
      pure l
    _ -> Akwan <$> liftRunMessage msg attrs
