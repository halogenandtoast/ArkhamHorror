module Arkham.Agenda.Cards.FloodedStreets (FloodedStreets (..), floodedStreets) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Trait (Trait (Suspect))

newtype FloodedStreets = FloodedStreets AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedStreets :: AgendaCard FloodedStreets
floodedStreets = agenda (3, A) FloodedStreets Cards.floodedStreets (Static 4)

instance HasModifiersFor FloodedStreets where
  getModifiersFor (FloodedStreets a) = do
    enemies <- modifySelect a (EnemyWithTrait Suspect) [IgnoreAloof]
    investigators <- modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait Suspect]
    pure $ enemies <> investigators

instance HasAbilities FloodedStreets where
  getAbilities (FloodedStreets a) = [mkAbility a 1 $ forced $ TurnEnds #when (You <> at_ FullyFloodedLocation)]

instance RunMessage FloodedStreets where
  runMessage msg a@(FloodedStreets attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach Anywhere (push . IncreaseFloodLevel)
      eachInvestigator (`drawEncounterCard` attrs)
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      randomDiscard iid (attrs.ability 1)
      pure a
    _ -> FloodedStreets <$> liftRunMessage msg attrs
