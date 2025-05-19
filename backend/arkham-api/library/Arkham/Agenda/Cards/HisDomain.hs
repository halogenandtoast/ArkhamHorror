module Arkham.Agenda.Cards.HisDomain (hisDomain) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as ActSequence
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Classes.HasQueue (removeAllMessagesMatching)
import Arkham.Deck qualified as Deck
import Arkham.Matcher hiding (InvestigatorDefeated, PlaceUnderneath)
import Arkham.Matcher qualified as Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HisDomain = HisDomain AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hisDomain :: AgendaCard HisDomain
hisDomain = agenda (3, A) HisDomain Cards.hisDomain (Static 8)

instance HasAbilities HisDomain where
  getAbilities (HisDomain attrs) =
    [ mkAbility attrs 1
        $ forced
        $ Matcher.PlaceUnderneath #when (TargetIs ActDeckTarget) (CardWithType EnemyType)
    ]

instance RunMessage HisDomain where
  runMessage msg a@(HisDomain attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push $ SetNoRemainingInvestigatorsHandler (toTarget attrs)
      eachInvestigator (investigatorDefeated attrs)
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 [windowType -> Window.PlaceUnderneath _ card] _ -> do
      lift $ removeAllMessagesMatching \case
        PlacedUnderneath ActDeckTarget card' -> card == card'
        CheckWindows [windowType -> Window.PlaceUnderneath ActDeckTarget card'] -> card == card'
        Do (CheckWindows [windowType -> Window.PlaceUnderneath ActDeckTarget card']) -> card == card'
        _ -> False
      shuffleCardsIntoDeck Deck.EncounterDeck (only card)
      pure a
    HandleNoRemainingInvestigators (isTarget attrs -> True) -> do
      anyResigned <- selectAny ResignedInvestigator
      if anyResigned
        then advanceToAct' attrs 1 Acts.noAsylum ActSequence.B
        else noResolution
      pure a
    _ -> HisDomain <$> liftRunMessage msg attrs
