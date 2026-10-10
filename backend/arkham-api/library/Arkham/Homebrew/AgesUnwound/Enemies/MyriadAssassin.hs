module Arkham.Homebrew.AgesUnwound.Enemies.MyriadAssassin (myriadAssassin) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (revelationSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards

newtype MyriadAssassin = MyriadAssassin EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

myriadAssassin :: EnemyCard MyriadAssassin
myriadAssassin = enemy MyriadAssassin Cards.myriadAssassin

{- | "Revelation - Test [agility] (3). If you fail, Myriad Assassin makes an
immediate attack against you."
-}
instance RunMessage MyriadAssassin where
  runMessage msg e@(MyriadAssassin attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure e
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      initiateEnemyAttack attrs attrs iid
      pure e
    _ -> MyriadAssassin <$> liftRunMessage msg attrs
