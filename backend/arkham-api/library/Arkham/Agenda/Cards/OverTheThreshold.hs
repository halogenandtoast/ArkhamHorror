module Arkham.Agenda.Cards.OverTheThreshold (
  OverTheThreshold (..),
  overTheThreshold,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner hiding (PhaseStep)
import Arkham.Card
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Types (Field (EnemyHealthDamage))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Act
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Trait (Trait (Humanoid, SilverTwilight, Spectral))

newtype OverTheThreshold = OverTheThreshold AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor OverTheThreshold where
  getModifiersFor (OverTheThreshold a) = do
    enemies <- modifySelect a (EnemyWithTrait SilverTwilight) [CountsAsInvestigatorForHunterEnemies]
    silverTwilight <- findAllCards (`cardMatch` CardWithTrait SilverTwilight)
    cards <- modifyEach a silverTwilight [GainVictory 0]
    pure $ enemies <> cards

overTheThreshold :: AgendaCard OverTheThreshold
overTheThreshold = agenda (2, A) OverTheThreshold Cards.overTheThreshold (Static 11)

instance HasAbilities OverTheThreshold where
  getAbilities (OverTheThreshold a) =
    [ restrictedAbility
        a
        1
        ( enemyExists
            $ ReadyEnemy
            <> EnemyWithTrait Spectral
            <> EnemyAt (LocationWithEnemy $ EnemyWithTrait Humanoid)
        )
        $ ForcedAbility
        $ PhaseStep #after HuntersMoveStep
    ]

instance RunMessage OverTheThreshold where
  runMessage msg a@(OverTheThreshold attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      step <- getCurrentActStep
      iids <- getInvestigators
      if step == 2
        then push R3
        else
          pushAll $ concatMap (\iid -> [SufferTrauma iid 1 0, InvestigatorDefeated (toSource attrs) iid]) iids
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      spectralEnemies <- selectWithField EnemyHealthDamage $ EnemyWithTrait Spectral <> ReadyEnemy
      enemyPairs <-
        catMaybes <$> for
          spectralEnemies
          \(enemy, damage) -> do
            humanoids <- select $ EnemyWithTrait Humanoid <> EnemyAt (locationWithEnemy enemy)
            pure $ guard (notNull humanoids) $> (enemy, damage, humanoids)
      lead <- getLeadPlayer
      pushWhen (notNull enemyPairs)
        $ chooseOrRunOneAtATime lead
        $ [ targetLabel enemy
            $ [ EnemyDamage humanoid $ nonAttack (EnemySource enemy) damage
              | humanoid <- humanoids
              ]
          | (enemy, damage, humanoids) <- enemyPairs
          ]
      pure a
    _ -> OverTheThreshold <$> runMessage msg attrs
