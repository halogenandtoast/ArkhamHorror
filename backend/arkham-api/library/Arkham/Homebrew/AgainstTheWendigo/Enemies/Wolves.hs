module Arkham.Homebrew.AgainstTheWendigo.Enemies.Wolves (wolves) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype Wolves = Wolves EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wolves :: EnemyCard Wolves
wolves = enemy Wolves Cards.wolves & setPrey (InvestigatorWithLowestSkill #willpower UneliminatedInvestigator)

instance HasAbilities Wolves where
  getAbilities (Wolves a) =
    [ restricted a 1 (notExists $ enemyIs Cards.wolves <> EnemyIsEngagedWith Anyone)
        $ forced
        $ PhaseEnds #when #enemy
    ]

instance RunMessage Wolves where
  runMessage msg e@(Wolves attrs) = runQueueT $ case msg of
    -- "If Wolves are not engaged at the end of the enemy phase: Shuffle Wolves
    -- into the encounter deck."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ ShuffleBackIntoEncounterDeck (toSource attrs) (toTarget attrs)
      pure e
    _ -> Wolves <$> liftRunMessage msg attrs
