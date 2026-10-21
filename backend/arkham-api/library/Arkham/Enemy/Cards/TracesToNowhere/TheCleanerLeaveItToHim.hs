module Arkham.Enemy.Cards.TracesToNowhere.TheCleanerLeaveItToHim (theCleanerLeaveItToHim) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Trait (Trait (Arkham))

newtype TheCleanerLeaveItToHim = TheCleanerLeaveItToHim EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCleanerLeaveItToHim :: EnemyCard TheCleanerLeaveItToHim
theCleanerLeaveItToHim =
  enemyWith TheCleanerLeaveItToHim Cards.theCleanerLeaveItToHim
    $ spawnAtL
    ?~ SpawnAtFirst
      [ SpawnAt $ arkhamWithClues <> not_ (LocationWithInvestigator Anyone)
      , SpawnAt arkhamWithClues
      ]

-- | "Spawn - Any Arkham location with 1 or more clues (and no investigators, if able)."
arkhamWithClues :: LocationMatcher
arkhamWithClues = LocationWithTrait Arkham <> LocationWithAnyClues

instance HasAbilities TheCleanerLeaveItToHim where
  getAbilities (TheCleanerLeaveItToHim a) =
    extend1 a $ restricted a 1 (thisExists a ReadyEnemy) $ forced $ PhaseEnds #when #enemy

instance RunMessage TheCleanerLeaveItToHim where
  runMessage msg e@(TheCleanerLeaveItToHim attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> flipCluesToDoom lid 1
      pure e
    _ -> TheCleanerLeaveItToHim <$> liftRunMessage msg attrs
