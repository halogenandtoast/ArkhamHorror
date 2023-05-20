module Arkham.Agenda.Cards.PendulousThreads (
  PendulousThreads (..),
  pendulousThreads,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding (InvestigatorEliminated)
import Arkham.Timing qualified as Timing

newtype PendulousThreads = PendulousThreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pendulousThreads :: AgendaCard PendulousThreads
pendulousThreads =
  agenda (2, A) PendulousThreads Cards.pendulousThreads (Static 7)

instance HasAbilities PendulousThreads where
  getAbilities (PendulousThreads a) =
    [ mkAbility a 1 $
        ForcedAbility $
          InvestigatorEliminated Timing.When $
            AnyInvestigator
              [ HandWith (HasCard $ CardWithTitle "Relic of Ages")
              , DiscardWith (HasCard $ CardWithTitle "Relic of Ages")
              , DeckWith (HasCard $ CardWithTitle "Relic of Ages")
              , HasMatchingAsset (AssetWithTitle "Relic of Ages")
              ]
    ]

instance RunMessage PendulousThreads where
  runMessage msg a@(PendulousThreads attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      inPlay <- selectAny $ enemyIs Enemies.formlessSpawn
      mVictory <-
        selectOne $
          VictoryDisplayCardMatch $
            cardIs
              Enemies.formlessSpawn
      nexus <- selectJust $ locationIs Locations.nexusOfNKai
      pushAll $
        [ShuffleEncounterDiscardBackIn]
          <> [PlaceDoom (toSource attrs) (toTarget nexus) 2 | inPlay]
          <> [ ShuffleCardsIntoDeck Deck.EncounterDeck [formlessSpawn]
             | formlessSpawn <- maybeToList mVictory
             ]
          <> [ AddToVictory (toTarget attrs)
             , advanceAgendaDeck attrs
             ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceToAgenda 1 Agendas.snappedThreads B (toSource attrs)
      pure a
    _ -> PendulousThreads <$> runMessage msg attrs
