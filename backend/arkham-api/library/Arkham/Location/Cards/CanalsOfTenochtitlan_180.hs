module Arkham.Location.Cards.CanalsOfTenochtitlan_180 (canalsOfTenochtitlan_180) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelfWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CanalsOfTenochtitlan_180 = CanalsOfTenochtitlan_180 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

canalsOfTenochtitlan_180 :: LocationCard CanalsOfTenochtitlan_180
canalsOfTenochtitlan_180 =
  symbolLabel $ location CanalsOfTenochtitlan_180 Cards.canalsOfTenochtitlan_180 5 (PerPlayer 1)

instance HasModifiersFor CanalsOfTenochtitlan_180 where
  getModifiersFor (CanalsOfTenochtitlan_180 a) = do
    modifySelect a (enemyAt a) [EnemyEvade 2]
    exhaustedEnemy <- selectAny $ ExhaustedEnemy <> enemyAt a
    modifySelfWhen a exhaustedEnemy [ShroudModifier (-3)]

instance RunMessage CanalsOfTenochtitlan_180 where
  runMessage msg (CanalsOfTenochtitlan_180 attrs) = CanalsOfTenochtitlan_180 <$> runMessage msg attrs
