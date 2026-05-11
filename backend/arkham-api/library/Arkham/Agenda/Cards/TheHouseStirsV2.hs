module Arkham.Agenda.Cards.TheHouseStirsV2 (theHouseStirsV2) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype TheHouseStirsV2 = TheHouseStirsV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseStirsV2 :: AgendaCard TheHouseStirsV2
theHouseStirsV2 = agenda (2, A) TheHouseStirsV2 Cards.theHouseStirsV2 (Static 5)

instance HasAbilities TheHouseStirsV2 where
  getAbilities (TheHouseStirsV2 a) =
    [mkAbility a 1 $ forced $ PhaseEnds #when #mythos]

instance RunMessage TheHouseStirsV2 where
  runMessage msg a@(TheHouseStirsV2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens lead (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheHouseStirsV2 <$> liftRunMessage msg attrs
