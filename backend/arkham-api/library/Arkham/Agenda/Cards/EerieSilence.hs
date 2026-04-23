module Arkham.Agenda.Cards.EerieSilence (eerieSilence) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype EerieSilence = EerieSilence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eerieSilence :: AgendaCard EerieSilence
eerieSilence = agenda (1, A) EerieSilence Cards.eerieSilence (Static 2)

instance HasAbilities EerieSilence where
  getAbilities (EerieSilence a) = [mkAbility a 1 actionAbility]

instance RunMessage EerieSilence where
  runMessage msg a@(EerieSilence attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens iid (attrs.ability 1) 1
      pure a
    _ -> EerieSilence <$> liftRunMessage msg attrs
