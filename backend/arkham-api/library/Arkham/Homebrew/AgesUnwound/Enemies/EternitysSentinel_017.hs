module Arkham.Homebrew.AgesUnwound.Enemies.EternitysSentinel_017 (eternitysSentinel_017) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype EternitysSentinel_017 = EternitysSentinel_017 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eternitysSentinel_017 :: EnemyCard EternitysSentinel_017
eternitysSentinel_017 = enemy EternitysSentinel_017 Cards.eternitysSentinel_017

{- | "Forced - At the end of the enemy phase, if Eternity's Sentinel is ready and
unengaged, and there is an investigator at Eternity's Sentinel's location: Place
1 doom on Eternity's Sentinel." / "Objective - If Eternity's Sentinel is
defeated: (-> R3)"

The doom is the investigators' clock running /backwards/: all three agendas
print "doom on cards other than this agenda subtracts from the total doom in
play", so the Watcher pacing itself in the corner is what buys them the night.
-}
instance HasAbilities EternitysSentinel_017 where
  getAbilities (EternitysSentinel_017 a) =
    extend
      a
      [ restricted
          a
          1
          ( thisExists a (ReadyEnemy <> UnengagedEnemy)
              <> exists (InvestigatorAt $ locationWithEnemy a.id)
          )
          $ forced
          $ PhaseEnds #when #enemy
      , mkAbility a 2 $ Objective $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage EternitysSentinel_017 where
  runMessage msg e@(EternitysSentinel_017 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R3
      pure e
    _ -> EternitysSentinel_017 <$> liftRunMessage msg attrs
