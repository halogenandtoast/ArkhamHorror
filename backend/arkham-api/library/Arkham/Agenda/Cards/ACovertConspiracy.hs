module Arkham.Agenda.Cards.ACovertConspiracy (aCovertConspiracy) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window.Enemy
import Arkham.Matcher
import Arkham.Scenarios.ByTheBook.Helpers
import Arkham.Trait (Trait (Cultist))

newtype ACovertConspiracy = ACovertConspiracy AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aCovertConspiracy :: AgendaCard ACovertConspiracy
aCovertConspiracy = agenda (1, A) ACovertConspiracy Cards.aCovertConspiracy (Static 6)

instance HasAbilities ACovertConspiracy where
  getAbilities (ACovertConspiracy a) =
    [ mkAbility a 1 $ forced $ EnemyWouldBeDefeated #when (NonWeaknessEnemy <> EnemyWithTrait Cultist)
    , mkAbility a 2 $ forced $ InvestigatorDefeated #when ByAny rolandBanks
    , mkAbility a 3 $ ActionAbility #resign Nothing (ActionCost 1)
    ]

instance RunMessage ACovertConspiracy where
  runMessage msg a@(ACovertConspiracy attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> eid) _ -> do
      healCultistInsteadOfDefeat (attrs.ability 1) eid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R2
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectOne rolandBanks >>= \case
        Just roland -> createEnemyCard_ Enemies.mrGrey roland
        Nothing -> do
          lead <- getLead
          createEnemyCard_ Enemies.mrGrey (locationWithInvestigator lead)
      advanceAgendaDeck attrs
      pure a
    _ -> ACovertConspiracy <$> liftRunMessage msg attrs
