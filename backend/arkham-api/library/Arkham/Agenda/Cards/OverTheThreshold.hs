module Arkham.Agenda.Cards.OverTheThreshold (overTheThreshold) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (PhaseStep)
import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyHealthDamage))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Act
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (SilverTwilight, Spectral))

newtype OverTheThreshold = OverTheThreshold AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor OverTheThreshold where
  getModifiersFor (OverTheThreshold a) = do
    modifySelect a (EnemyWithTrait SilverTwilight) [CountsAsInvestigatorForHunterEnemies]
    silverTwilight <- findAllCards (`cardMatch` CardWithTrait SilverTwilight)
    modifyEach a silverTwilight [GainVictory 0]

overTheThreshold :: AgendaCard OverTheThreshold
overTheThreshold = agenda (2, A) OverTheThreshold Cards.overTheThreshold (Static 11)

instance HasAbilities OverTheThreshold where
  getAbilities (OverTheThreshold a) =
    [ restricted a 1 (exists $ ReadyEnemy <> EnemyWithTrait Spectral <> at_ (LocationWithEnemy #humanoid))
        $ forced
        $ PhaseStep #after HuntersMoveStep
    ]

instance RunMessage OverTheThreshold where
  runMessage msg a@(OverTheThreshold attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      step <- getCurrentActStep
      if step == 2
        then push R3
        else eachInvestigator \iid -> do
          sufferPhysicalTrauma iid 1
          investigatorDefeated attrs iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      spectralEnemies <- selectWithField EnemyHealthDamage $ EnemyWithTrait Spectral <> ReadyEnemy
      enemyPairs <-
        catMaybes <$> for spectralEnemies \(enemy, damage) -> do
          humanoids <- select $ #humanoid <> EnemyAt (locationWithEnemy enemy)
          pure $ guard (notNull humanoids) $> (enemy, damage, humanoids)
      lead <- getLead
      chooseOrRunOneAtATimeM lead do
        for_ enemyPairs \(enemy, damage, humanoids) -> do
          targeting enemy $ for_ humanoids $ nonAttackEnemyDamage Nothing enemy damage
      pure a
    _ -> OverTheThreshold <$> liftRunMessage msg attrs
