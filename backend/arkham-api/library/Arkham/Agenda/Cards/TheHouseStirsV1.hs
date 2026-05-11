module Arkham.Agenda.Cards.TheHouseStirsV1 (theHouseStirsV1) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype TheHouseStirsV1 = TheHouseStirsV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseStirsV1 :: AgendaCard TheHouseStirsV1
theHouseStirsV1 = agenda (2, A) TheHouseStirsV1 Cards.theHouseStirsV1 (Static 5)

instance HasAbilities TheHouseStirsV1 where
  getAbilities (TheHouseStirsV1 a) =
    [ -- "Forced - When the mythos phase ends: Make a predation test."
      mkAbility a 1
        $ forced
        $ PhaseEnds #when #mythos
    ]

instance RunMessage TheHouseStirsV1 where
  runMessage msg a@(TheHouseStirsV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens lead (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheHouseStirsV1 <$> liftRunMessage msg attrs
