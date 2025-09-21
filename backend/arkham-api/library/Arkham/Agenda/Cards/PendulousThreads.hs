module Arkham.Agenda.Cards.PendulousThreads (pendulousThreads) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype PendulousThreads = PendulousThreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pendulousThreads :: AgendaCard PendulousThreads
pendulousThreads = agenda (2, A) PendulousThreads Cards.pendulousThreads (Static 7)

instance HasAbilities PendulousThreads where
  getAbilities (PendulousThreads a) =
    [ mkAbility a 1
        $ forced
        $ InvestigatorEliminated #when
        $ oneOf
          [ HandWith (HasCard "Relic of Ages")
          , DiscardWith (HasCard "Relic of Ages")
          , DeckWith (HasCard "Relic of Ages")
          , HasMatchingAsset "Relic of Ages"
          ]
    ]

instance RunMessage PendulousThreads where
  runMessage msg a@(PendulousThreads attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn

      whenAny (enemyIs Enemies.formlessSpawn) do
        nexus <- selectJust $ locationIs Locations.nexusOfNKai
        placeDoom attrs nexus 2

      mVictory <- selectOne $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.formlessSpawn
      for_ mVictory \formlessSpawn -> shuffleCardsIntoDeck Deck.EncounterDeck [formlessSpawn]
      addToVictory attrs
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceToAgenda attrs Cards.snappedThreads B
      pure a
    _ -> PendulousThreads <$> liftRunMessage msg attrs
