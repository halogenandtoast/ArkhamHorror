module Arkham.Homebrew.DarkMatter.Agendas.FigmentOfYourImagination (figmentOfYourImagination) where

import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Query (getSetAsideEncounterSet)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Sets qualified as Set

newtype FigmentOfYourImagination = FigmentOfYourImagination AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

figmentOfYourImagination :: AgendaCard FigmentOfYourImagination
figmentOfYourImagination = agenda (1, A) FigmentOfYourImagination Cards.figmentOfYourImagination (Static 4)

instance RunMessage FigmentOfYourImagination where
  runMessage msg a@(FigmentOfYourImagination attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      step <- getCurrentActStep
      for_ (lookup step [(1, Locations.schoolGrounds), (2, Locations.entranceHall)]) \location ->
        createSetAsideEnemyWith_ Enemies.theBOOGEYMAN location createExhausted

      boogeymanSet <- getSetAsideEncounterSet Set.TheBoogeyman
      shuffleCardsIntoDeck Deck.EncounterDeck
        $ filter ((/= toCardCode Enemies.theBOOGEYMAN) . toCardCode) boogeymanSet
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> FigmentOfYourImagination <$> liftRunMessage msg attrs
