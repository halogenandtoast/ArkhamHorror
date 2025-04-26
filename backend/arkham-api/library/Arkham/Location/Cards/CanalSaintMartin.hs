module Arkham.Location.Cards.CanalSaintMartin (canalSaintMartin) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype CanalSaintMartin = CanalSaintMartin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canalSaintMartin :: LocationCard CanalSaintMartin
canalSaintMartin = location CanalSaintMartin Cards.canalSaintMartin 4 (PerPlayer 1)

instance HasAbilities CanalSaintMartin where
  getAbilities (CanalSaintMartin a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ freeReaction
      $ Matcher.EnemyEvaded #after You (at_ $ be a)

instance RunMessage CanalSaintMartin where
  runMessage msg a@(CanalSaintMartin attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (evadedEnemy -> eid) _ -> do
      connectingLocations <- select $ ConnectedFrom (be attrs) <> LocationCanBeEnteredBy eid
      chooseTargetM iid connectingLocations $ enemyMoveTo eid
      pure a
    _ -> CanalSaintMartin <$> liftRunMessage msg attrs
