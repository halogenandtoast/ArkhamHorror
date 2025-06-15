module Arkham.Enemy.Cards.SavageShantak (
  savageShantak,
  SavageShantak(..),
) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype SavageShantak = SavageShantak EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

savageShantak :: EnemyCard SavageShantak
savageShantak = enemy SavageShantak Cards.savageShantak (1, Static 4, 1) (2, 1)

instance HasModifiersFor SavageShantak where
  getModifiersFor (SavageShantak a) = do
    mLocation <- selectOne $ locationWithEnemy a
    guestCount <- case mLocation of
      Nothing -> pure 0
      Just lid -> selectCount $ AssetWithTrait Guest <> assetAt lid
    modifySelf a [EnemyFight guestCount, EnemyEvade guestCount]

instance HasAbilities SavageShantak where
  getAbilities = enemyAbilities

instance RunMessage SavageShantak where
  runMessage msg (SavageShantak attrs) =
    SavageShantak <$> runMessage msg attrs
