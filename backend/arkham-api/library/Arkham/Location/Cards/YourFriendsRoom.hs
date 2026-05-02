module Arkham.Location.Cards.YourFriendsRoom (yourFriendsRoom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (yourFriendsRoom)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype YourFriendsRoom = YourFriendsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yourFriendsRoom :: LocationCard YourFriendsRoom
yourFriendsRoom = location YourFriendsRoom Cards.yourFriendsRoom 2 (PerPlayer 2)

instance HasAbilities YourFriendsRoom where
  getAbilities (YourFriendsRoom a) =
    extendRevealed1 a
      $ noAOO
      $ restricted a 1 (Here <> exists (at_ (connectedFrom (be a)) <> UnengagedEnemy)) actionAbility

instance RunMessage YourFriendsRoom where
  runMessage msg l@(YourFriendsRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt $ connectedFrom (be attrs)
      chooseTargetM iid enemies \enemy -> do
        enemyMoveTo (attrs.ability 1) enemy (toId attrs)
        engageEnemy iid enemy
      pure l
    _ -> YourFriendsRoom <$> liftRunMessage msg attrs
