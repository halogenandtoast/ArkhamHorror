module Arkham.Enemy.Cards.WingedKeeper (wingedKeeper) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTowardsMatching)

newtype WingedKeeper = WingedKeeper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wingedKeeper :: EnemyCard WingedKeeper
wingedKeeper =
  enemy WingedKeeper Cards.wingedKeeper
    & setSpawnAt (HighestRow LocationWithoutInvestigators)
    & setPrey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasAbilities WingedKeeper where
  getAbilities (WingedKeeper a) =
    extend1 a
      $ restricted a 1 (exists $ be a <> ReadyEnemy <> UnengagedEnemy)
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage WingedKeeper where
  runMessage msg e@(WingedKeeper attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      preyIds <- select (enemyPrey attrs)
      moveTowardsMatching
        (attrs.ability 1)
        attrs
        (LocationWithInvestigator $ oneOf $ map InvestigatorWithId preyIds)
      pure e
    _ -> WingedKeeper <$> liftRunMessage msg attrs
