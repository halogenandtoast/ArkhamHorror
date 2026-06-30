module Arkham.Agenda.Cards.SilenusDescends (silenusDescends) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype SilenusDescends = SilenusDescends AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silenusDescends :: AgendaCard SilenusDescends
silenusDescends = agenda (4, A) SilenusDescends Cards.silenusDescends (Static 9)

instance HasModifiersFor SilenusDescends where
  getModifiersFor (SilenusDescends a) =
    modifySelect
      a
      (factionEnemy BlueFaction)
      [RemoveKeyword Keyword.Aloof, RemoveKeyword #warring, AddKeyword Keyword.Hunter]

instance HasAbilities SilenusDescends where
  getAbilities (SilenusDescends a) =
    [mkAbility a 1 $ Objective $ forced $ IfEnemyDefeated_ #after (enemyIs Enemies.silenus)]

instance RunMessage SilenusDescends where
  runMessage msg a@(SilenusDescends attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R3
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R6
      pure a
    _ -> SilenusDescends <$> liftRunMessage msg attrs
