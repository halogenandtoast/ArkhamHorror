module Arkham.Enemy.Cards.ChildrenOfBlood.RiverOfBlood.WaterfrontCivilian (waterfrontCivilian) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Arkham))

newtype WaterfrontCivilian = WaterfrontCivilian EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

waterfrontCivilian :: EnemyCard WaterfrontCivilian
waterfrontCivilian =
  enemy WaterfrontCivilian Cards.waterfrontCivilian
    & setSpawnAt (EmptyLocation <> LocationWithTrait Arkham)

instance RunMessage WaterfrontCivilian where
  runMessage msg (WaterfrontCivilian attrs) = runQueueT $ case msg of
    _ -> WaterfrontCivilian <$> liftRunMessage msg attrs
