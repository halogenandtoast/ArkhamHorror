module Arkham.Act.Cards.FriendsInHighPlacesHenryDeveau
  ( FriendsInHighPlacesHenryDeveau(..)
  , friendsInHighPlacesHenryDeveau
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype FriendsInHighPlacesHenryDeveau = FriendsInHighPlacesHenryDeveau ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

friendsInHighPlacesHenryDeveau :: ActCard FriendsInHighPlacesHenryDeveau
friendsInHighPlacesHenryDeveau = act
  (2, C)
  FriendsInHighPlacesHenryDeveau
  Cards.friendsInHighPlacesHenryDeveau
  Nothing

instance RunMessage FriendsInHighPlacesHenryDeveau where
  runMessage msg (FriendsInHighPlacesHenryDeveau attrs) =
    FriendsInHighPlacesHenryDeveau <$> runMessage msg attrs
