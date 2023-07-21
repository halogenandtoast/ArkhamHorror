module Arkham.Agenda.Cards.HisDomain (
  HisDomain (..),
  hisDomain,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as ActSequence
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Matcher hiding (InvestigatorDefeated, PlaceUnderneath)
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HisDomain = HisDomain AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hisDomain :: AgendaCard HisDomain
hisDomain = agenda (3, A) HisDomain Cards.hisDomain (Static 8)

instance HasAbilities HisDomain where
  getAbilities (HisDomain attrs) =
    [ mkAbility attrs 1 $
        ForcedAbility $
          Matcher.PlaceUnderneath
            Timing.When
            (TargetIs ActDeckTarget)
            (CardWithType EnemyType)
    ]

instance RunMessage HisDomain where
  runMessage msg a@(HisDomain attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      investigatorIds <- selectList UneliminatedInvestigator
      a
        <$ pushAll
          ( SetNoRemainingInvestigatorsHandler (toTarget attrs)
              : map (InvestigatorDefeated (toSource attrs)) investigatorIds
          )
    UseCardAbility _ source 1 [Window _ (Window.PlaceUnderneath _ card)] _
      | isSource attrs source -> do
          removeAllMessagesMatching \case
            PlacedUnderneath ActDeckTarget card' -> card == card'
            CheckWindow _ [Window _ (Window.PlaceUnderneath ActDeckTarget card')] ->
              card == card'
            _ -> False
          push $ ShuffleCardsIntoDeck Deck.EncounterDeck [card]
          pure a
    HandleNoRemainingInvestigators target | isTarget attrs target -> do
      anyResigned <- selectAny ResignedInvestigator
      if anyResigned
        then
          a
            <$ push
              (AdvanceToAct 1 Acts.noAsylum ActSequence.B (toSource attrs))
        else a <$ push (ScenarioResolution NoResolution)
    _ -> HisDomain <$> runMessage msg attrs
