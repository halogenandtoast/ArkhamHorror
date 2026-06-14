module Arkham.Agenda.Cards.FloodedArchives (floodedArchives) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (struggleForAir)
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (decreaseThisFloodLevel)
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario (getEncounterDiscard)
import Arkham.Matcher
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (RegularEncounterDeck))
import Arkham.Trait (Trait (Glyph, Omen))

newtype FloodedArchives = FloodedArchives AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedArchives :: AgendaCard FloodedArchives
floodedArchives = agenda (1, A) FloodedArchives Cards.floodedArchives (Static 6)

instance HasAbilities FloodedArchives where
  getAbilities (FloodedArchives a) =
    [ restricted a 1 (youExist $ at_ FullyFloodedLocation)
        $ forced
        $ TurnBegins #when You
    ]

instance RunMessage FloodedArchives where
  runMessage msg a@(FloodedArchives attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      flooded <- select FloodedLocation
      for_ flooded decreaseThisFloodLevel
      omensAndGlyphs <-
        filterCards (mapOneOf CardWithTrait [Omen, Glyph])
          <$> getEncounterDiscard RegularEncounterDeck
      shuffleCardsIntoDeck Deck.EncounterDeck omensAndGlyphs
      advanceAgendaDeck attrs
      pure a
    _ -> FloodedArchives <$> liftRunMessage msg attrs
