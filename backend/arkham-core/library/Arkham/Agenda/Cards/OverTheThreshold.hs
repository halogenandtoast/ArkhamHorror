module Arkham.Agenda.Cards.OverTheThreshold (
  OverTheThreshold (..),
  overTheThreshold,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner hiding (PhaseStep)
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Types (Field (EnemyHealthDamage))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Act
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Trait (Trait (Humanoid, SilverTwilight, Spectral), toTraits)

newtype OverTheThreshold = OverTheThreshold AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor OverTheThreshold where
  getModifiersFor (EnemyTarget eid) (OverTheThreshold a) = do
    isSilverTwilight <- eid <=~> EnemyWithTrait SilverTwilight
    pure $ toModifiers a [CountsAsInvestigatorForHunterEnemies | isSilverTwilight]
  getModifiersFor (CardIdTarget cid) (OverTheThreshold a) = do
    card <- getCard cid
    let isSilverTwilight = SilverTwilight `member` toTraits card
    pure $ toModifiers a [GainVictory 0 | isSilverTwilight]
  getModifiersFor _ _ = pure []

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
      iids <- getInvestigatorIds
      if step == 2
        then push R3
        else pushAll $ concatMap (\iid -> [SufferTrauma iid 1 0, InvestigatorDefeated (toSource attrs) iid]) iids
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      spectralEnemies <- selectWithField EnemyHealthDamage $ EnemyWithTrait Spectral <> ReadyEnemy
      enemyPairs <-
        catMaybes <$> for
          spectralEnemies
          \(enemy, damage) -> do
            humanoids <- selectList $ EnemyWithTrait Humanoid <> EnemyAt (locationWithEnemy enemy)
            pure $ guard (notNull humanoids) $> (enemy, damage, humanoids)
      lead <- getLead
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
