module Arkham.Agenda.Cards.JourneyThroughTheGates (journeyThroughTheGates) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.Helpers.Act
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Trait (Trait (Steps))

newtype JourneyThroughTheGates = JourneyThroughTheGates AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

journeyThroughTheGates :: AgendaCard JourneyThroughTheGates
journeyThroughTheGates = agenda (1, A) JourneyThroughTheGates Cards.journeyThroughTheGates (Static 19)

instance HasAbilities JourneyThroughTheGates where
  getAbilities (JourneyThroughTheGates a) =
    [ restrictedAbility a 1 (exists $ InvestigatorAt $ LocationWithTrait Steps)
        $ forced
        $ PlacedDoomCounter #when AnySource
        $ TargetIs
        $ toTarget a
    ]

instance RunMessage JourneyThroughTheGates where
  runMessage msg a@(JourneyThroughTheGates attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      step <- getCurrentActStep
      if step == 4
        then do
          record TheDreamersStrayedFromThePath
          eachInvestigator \iid -> do
            sufferMentalTrauma iid 1
            investigatorDefeated attrs iid
        else do
          eachInvestigator \iid -> do
            sid <- genId
            chooseOneM iid do
              labeled
                "Test {willpower} (3) to remember that this is all a dream"
                $ beginSkillTest sid iid attrs iid #willpower (Fixed 3)
              labeled "Do not test" do
                sufferPhysicalTrauma iid 1
                investigatorDefeated attrs iid
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      sufferMentalTrauma iid 1
      investigatorDefeated attrs iid
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      sufferPhysicalTrauma iid 1
      investigatorDefeated attrs iid
      pure a
    _ -> JourneyThroughTheGates <$> liftRunMessage msg attrs
