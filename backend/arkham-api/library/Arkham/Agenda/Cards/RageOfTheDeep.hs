module Arkham.Agenda.Cards.RageOfTheDeep (RageOfTheDeep (..), rageOfTheDeep) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Trait (Trait (Suspect))

newtype RageOfTheDeep = RageOfTheDeep AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rageOfTheDeep :: AgendaCard RageOfTheDeep
rageOfTheDeep = agenda (4, A) RageOfTheDeep Cards.rageOfTheDeep (Static 12)

instance HasModifiersFor RageOfTheDeep where
  getModifiersFor (RageOfTheDeep a) = do
    enemies <- modifySelect a (EnemyWithTrait Suspect) [IgnoreAloof]
    investigators <- modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait Suspect]
    pure $ enemies <> investigators

instance HasAbilities RageOfTheDeep where
  getAbilities (RageOfTheDeep a) = [mkAbility a 1 $ forced $ TurnEnds #when (You <> at_ FullyFloodedLocation)]

instance RunMessage RageOfTheDeep where
  runMessage msg a@(RageOfTheDeep attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      record InnsmouthWasConsumedByTheRisingTide
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        push $ InvestigatorDefeated (toSource attrs) iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      randomDiscardN iid (attrs.ability 1) 2
      pure a
    _ -> RageOfTheDeep <$> liftRunMessage msg attrs
