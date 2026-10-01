module Arkham.Homebrew.CircusExMortis.Enemies.RoamingDarkYoung (roamingDarkYoung) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken, sealMoonTokenOn)
import Arkham.Matcher

newtype RoamingDarkYoung = RoamingDarkYoung EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Spawn - Any empty location." EmptyLocation is no investigators and no enemies; the
drawing investigator picks, and the engine discards the enemy when none matches.
-}
roamingDarkYoung :: EnemyCard RoamingDarkYoung
roamingDarkYoung = enemyWith RoamingDarkYoung Cards.roamingDarkYoung spawnAtEmptyLocation

instance HasAbilities RoamingDarkYoung where
  getAbilities (RoamingDarkYoung a) =
    extend1 a
      $ restricted a 1 (exists moonToken)
      $ forced
      $ Enters #after You (locationWithEnemy a)

instance RunMessage RoamingDarkYoung where
  runMessage msg e@(RoamingDarkYoung attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sealMoonTokenOn iid
      pure e
    _ -> RoamingDarkYoung <$> liftRunMessage msg attrs
