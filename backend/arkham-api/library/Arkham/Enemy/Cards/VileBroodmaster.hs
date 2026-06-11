module Arkham.Enemy.Cards.VileBroodmaster (vileBroodmaster) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Token

newtype VileBroodmaster = VileBroodmaster EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vileBroodmaster :: EnemyCard VileBroodmaster
vileBroodmaster = enemy VileBroodmaster Cards.vileBroodmaster (4, Static 7, 3) (2, 1)

instance HasModifiersFor VileBroodmaster where
  getModifiersFor (VileBroodmaster a) = do
    mutations <- getMutations a.id
    modifySelf a [HealthModifier mutations | mutations > 0]

instance RunMessage VileBroodmaster where
  runMessage msg (VileBroodmaster attrs) = runQueueT $ case msg of
    -- When 1 or more mutations would be placed on an enemy, place 1
    -- additional mutation on that enemy.
    PlaceTokens source (EnemyTarget eid) Resource n | n > 0 && source /= toSource attrs -> do
      placeMutations attrs eid 1
      VileBroodmaster <$> liftRunMessage msg attrs
    _ -> VileBroodmaster <$> liftRunMessage msg attrs
