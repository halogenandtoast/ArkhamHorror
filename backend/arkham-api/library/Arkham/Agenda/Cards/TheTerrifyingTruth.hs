module Arkham.Agenda.Cards.TheTerrifyingTruth (theTerrifyingTruth) where

import Arkham.Ability
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Story
import Arkham.Matcher

newtype TheTerrifyingTruth = TheTerrifyingTruth AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTerrifyingTruth :: AgendaCard TheTerrifyingTruth
theTerrifyingTruth = agenda (2, A) TheTerrifyingTruth Cards.theTerrifyingTruth (Static 3)

instance HasAbilities TheTerrifyingTruth where
  getAbilities (TheTerrifyingTruth a) =
    [ mkAbility a 1 $ forced $ AgendaWouldAdvance #when DoomThreshold (be a)
    | onSide A a
    ]

instance RunMessage TheTerrifyingTruth where
  runMessage msg a@(TheTerrifyingTruth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      storyCards <- select $ UnderScenarioReferenceMatch $ CardWithType StoryType
      case nonEmpty storyCards of
        Nothing -> advanceAgenda attrs
        Just xs -> do
          card <- sample xs
          obtainCard card
          readStory iid card (toCardDef card)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R3
      pure a
    _ -> TheTerrifyingTruth <$> liftRunMessage msg attrs
