module Arkham.Agenda.Cards.AroundTheTable (aroundTheTable) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype AroundTheTable = AroundTheTable AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aroundTheTable :: AgendaCard AroundTheTable
aroundTheTable = agenda (1, A) AroundTheTable Cards.aroundTheTable (Static 6)

instance HasAbilities AroundTheTable where
  getAbilities (AroundTheTable x) =
    [mkAbility x 1 $ Objective $ forced $ ifEnemyDefeated Enemies.motherRachelStarbornHerald]

instance RunMessage AroundTheTable where
  runMessage msg a@(AroundTheTable attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeled' "sufferPhysicalTrauma" $ sufferPhysicalTrauma iid 1
          countVar 1 $ labeled' "sufferMentalTrauma" $ sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push R1
      pure a
    _ -> AroundTheTable <$> liftRunMessage msg attrs
