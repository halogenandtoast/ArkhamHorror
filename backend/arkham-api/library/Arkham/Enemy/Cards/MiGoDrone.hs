module Arkham.Enemy.Cards.MiGoDrone (miGoDrone) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype MiGoDrone = MiGoDrone EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoDrone :: EnemyCard MiGoDrone
miGoDrone =
  enemyWith
    MiGoDrone
    Cards.miGoDrone
    (spawnAtL ?~ SpawnAt (LocationWithMostClues $ not_ $ LocationWithTitle "Fungus Mound"))

instance HasAbilities MiGoDrone where
  getAbilities (MiGoDrone a) = extend1 a $ restricted a 1 (thisIs a ReadyEnemy) $ forced $ PhaseBegins #when #enemy

instance HasModifiersFor MiGoDrone where
  getModifiersFor (MiGoDrone a) = do
    theSecretOfTheOozeWasStolen <- remembered TheSecretOfTheOozeWasStolen
    modifySelfWhen a theSecretOfTheOozeWasStolen [HealthModifier 2, EnemyEvade 2]

instance RunMessage MiGoDrone where
  runMessage msg e@(MiGoDrone attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      fungusMound <- selectJust $ LocationWithTitle "Fungus Mound"
      selectOne (locationWithEnemy attrs) >>= traverse_ \lid -> whenMatch lid LocationWithAnyClues $ moveTokens (attrs.ability 1) lid fungusMound #clue 1
      pure e
    _ -> MiGoDrone <$> liftRunMessage msg attrs
