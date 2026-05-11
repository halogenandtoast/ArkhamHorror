module Arkham.Agenda.Cards.LivingWalls (livingWalls) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype LivingWalls = LivingWalls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingWalls :: AgendaCard LivingWalls
livingWalls = agenda (3, A) LivingWalls Cards.livingWalls (Static 8)

instance HasAbilities LivingWalls where
  getAbilities (LivingWalls a) =
    [mkAbility a 1 $ forced $ PhaseEnds #when #mythos]

instance RunMessage LivingWalls where
  runMessage msg a@(LivingWalls attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens lead (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> LivingWalls <$> liftRunMessage msg attrs
