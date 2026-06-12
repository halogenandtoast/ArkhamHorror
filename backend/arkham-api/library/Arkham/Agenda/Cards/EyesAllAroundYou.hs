module Arkham.Agenda.Cards.EyesAllAroundYou (eyesAllAroundYou) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.AllOrNothing.Helpers

newtype EyesAllAroundYou = EyesAllAroundYou AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesAllAroundYou :: AgendaCard EyesAllAroundYou
eyesAllAroundYou = agenda (1, A) EyesAllAroundYou Cards.eyesAllAroundYou (Static 11)

instance HasAbilities EyesAllAroundYou where
  getAbilities (EyesAllAroundYou a)
    | onSide A a =
        [ restricted a 1 (OnLocation (locationIs Locations.cloverClubCardroom))
            $ actionAbilityWithCost ClueCostX
        , mkAbility a 2 $ forced $ InvestigatorDefeated #when ByAny skidsOToole
        ]
    | otherwise = []

instance RunMessage EyesAllAroundYou where
  runMessage msg a@(EyesAllAroundYou attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePayment -> x) -> do
      gainResources iid (attrs.ability 1) (5 * x)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R2
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push $ SetNoRemainingInvestigatorsHandler (toTarget attrs)
      selectEach (not_ ResignedInvestigator) \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    HandleNoRemainingInvestigators (isTarget attrs -> True) -> do
      skidsResigned <- selectAny $ ResignedInvestigator <> skidsOToole
      push $ if skidsResigned then R1 else R2
      pure a
    _ -> EyesAllAroundYou <$> liftRunMessage msg attrs
