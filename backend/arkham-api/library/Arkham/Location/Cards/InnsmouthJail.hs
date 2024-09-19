module Arkham.Location.Cards.InnsmouthJail (innsmouthJail, InnsmouthJail (..)) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype InnsmouthJail = InnsmouthJail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthJail :: LocationCard InnsmouthJail
innsmouthJail = location InnsmouthJail Cards.innsmouthJail 4 (PerPlayer 1)

instance HasAbilities InnsmouthJail where
  getAbilities (InnsmouthJail a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced (EnemyDefeated #when You ByAny AnyEnemy)

instance RunMessage InnsmouthJail where
  runMessage msg l@(InnsmouthJail attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      push $ RemoveEnemy enemy
      shuffleIntoLeadsDeck . only =<< field EnemyCard enemy
      pure l
    _ -> InnsmouthJail <$> liftRunMessage msg attrs
