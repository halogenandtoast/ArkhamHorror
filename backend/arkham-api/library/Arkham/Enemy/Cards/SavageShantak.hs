module Arkham.Enemy.Cards.SavageShantak (savageShantak) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Trait (Trait (Guest))

newtype SavageShantak = SavageShantak EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

savageShantak :: EnemyCard SavageShantak
savageShantak = enemy SavageShantak Cards.savageShantak (1, Static 4, 1) (2, 1)

instance HasModifiersFor SavageShantak where
  getModifiersFor (SavageShantak a) = do
    mLocation <- selectOne $ locationWithEnemy a
    guestCount <- case mLocation of
      Nothing -> pure 0
      Just lid -> selectCount $ AssetWithTrait Guest <> assetAt lid
    modifySelf a [EnemyFight guestCount, EnemyEvade guestCount]

instance RunMessage SavageShantak where
  runMessage msg (SavageShantak attrs) =
    SavageShantak <$> runMessage msg attrs
