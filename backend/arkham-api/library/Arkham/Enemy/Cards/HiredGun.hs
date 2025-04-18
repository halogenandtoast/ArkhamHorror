module Arkham.Enemy.Cards.HiredGun (hiredGun) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (Surge)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Trait (Trait(Criminal))
import Arkham.Treachery.Cards qualified as Treacheries

newtype HiredGun = HiredGun EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hiredGun :: EnemyCard HiredGun
hiredGun = enemy HiredGun Cards.hiredGun (3, Static 4, 1) (1, 1)

instance HasModifiersFor HiredGun where
  getModifiersFor (HiredGun a) = do
    modifySelect a (EnemyWithTrait Criminal) [AddKeyword Hunter, ForcePrey (Prey MostResources)]
    huntedDown <- findAllCards (`isCard` Treacheries.huntedDown)
    modifyEach a huntedDown [AddKeyword Surge, AddKeyword Peril]

instance RunMessage HiredGun where
  runMessage msg (HiredGun attrs) = runQueueT $ case msg of
    _ -> HiredGun <$> liftRunMessage msg attrs
