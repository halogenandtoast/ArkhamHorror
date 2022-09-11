module Arkham.Act.Cards.FriendsInHighPlacesHenrysInformation
  ( FriendsInHighPlacesHenrysInformation(..)
  , friendsInHighPlacesHenrysInformation
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype FriendsInHighPlacesHenrysInformation = FriendsInHighPlacesHenrysInformation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

friendsInHighPlacesHenrysInformation
  :: ActCard FriendsInHighPlacesHenrysInformation
friendsInHighPlacesHenrysInformation = act
  (2, C)
  FriendsInHighPlacesHenrysInformation
  Cards.friendsInHighPlacesHenrysInformation
  Nothing

instance RunMessage FriendsInHighPlacesHenrysInformation where
  runMessage msg (FriendsInHighPlacesHenrysInformation attrs) =
    FriendsInHighPlacesHenrysInformation <$> runMessage msg attrs
