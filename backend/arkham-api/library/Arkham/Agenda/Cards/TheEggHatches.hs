module Arkham.Agenda.Cards.TheEggHatches (theEggHatches) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype TheEggHatches = TheEggHatches AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEggHatches :: AgendaCard TheEggHatches
theEggHatches = agenda (4, A) TheEggHatches Cards.theEggHatches (Static 9)

instance HasModifiersFor TheEggHatches where
  getModifiersFor (TheEggHatches a) =
    modifySelect
      a
      (factionEnemy GreenFaction)
      [RemoveKeyword Keyword.Aloof, RemoveKeyword #warring, AddKeyword Keyword.Hunter]

instance HasAbilities TheEggHatches where
  getAbilities (TheEggHatches a) =
    [mkAbility a 1 $ Objective $ forced $ IfEnemyDefeated_ #after (enemyIs Enemies.maghanArkat)]

instance RunMessage TheEggHatches where
  runMessage msg a@(TheEggHatches attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R4
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R7
      pure a
    _ -> TheEggHatches <$> liftRunMessage msg attrs
