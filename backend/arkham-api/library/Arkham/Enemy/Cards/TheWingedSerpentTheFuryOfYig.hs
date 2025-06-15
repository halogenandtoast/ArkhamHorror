module Arkham.Enemy.Cards.TheWingedSerpentTheFuryOfYig (theWingedSerpentTheFuryOfYig) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Token

newtype TheWingedSerpentTheFuryOfYig = TheWingedSerpentTheFuryOfYig EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWingedSerpentTheFuryOfYig :: EnemyCard TheWingedSerpentTheFuryOfYig
theWingedSerpentTheFuryOfYig =
  enemyWith TheWingedSerpentTheFuryOfYig Cards.theWingedSerpentTheFuryOfYig (4, Static 1, 4) (1, 1)
    $ (spawnAtL ?~ SpawnAt (LocationWithTitle "Mouth of K'n-yan"))
    . (healthL .~ Nothing)

instance HasModifiersFor TheWingedSerpentTheFuryOfYig where
  getModifiersFor (TheWingedSerpentTheFuryOfYig a) = modifySelf a [CannotBeDefeated, CannotMakeAttacksOfOpportunity]

instance HasAbilities TheWingedSerpentTheFuryOfYig where
  getAbilities (TheWingedSerpentTheFuryOfYig a) =
    extend
      a
      [ groupLimit PerTestOrAbility
          $ restricted a 1 (thisExists a (EnemyWithDamage $ atLeast 5))
          $ forced AnyWindow
      , mkAbility a 1
          $ forced
          $ PlacedToken
            #after
            AnySource
            (LocationTargetMatches $ locationIs Locations.mouthOfKnYanTheCavernsMaw)
            Pillar
      ]

instance RunMessage TheWingedSerpentTheFuryOfYig where
  runMessage msg e@(TheWingedSerpentTheFuryOfYig attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healAllDamage (attrs.ability 1) attrs
      exhaustThis attrs
      nextPhaseModifier #upkeep (attrs.ability 1) attrs DoesNotReadyDuringUpkeep
      pure e
    _ -> TheWingedSerpentTheFuryOfYig <$> liftRunMessage msg attrs
