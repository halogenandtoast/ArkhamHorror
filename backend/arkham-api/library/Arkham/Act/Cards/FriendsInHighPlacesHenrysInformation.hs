module Arkham.Act.Cards.FriendsInHighPlacesHenrysInformation (friendsInHighPlacesHenrysInformation) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement

newtype FriendsInHighPlacesHenrysInformation = FriendsInHighPlacesHenrysInformation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

friendsInHighPlacesHenrysInformation
  :: ActCard FriendsInHighPlacesHenrysInformation
friendsInHighPlacesHenrysInformation =
  act (2, C) FriendsInHighPlacesHenrysInformation Cards.friendsInHighPlacesHenrysInformation Nothing

instance HasAbilities FriendsInHighPlacesHenrysInformation where
  getAbilities = actAbilities1' C \a ->
    restricted a 1 (exists $ assetIs Assets.henryDeveau <> AssetWithClues (AtLeast $ PerPlayer 1))
      $ Objective
      $ forced AnyWindow

instance RunMessage FriendsInHighPlacesHenrysInformation where
  runMessage msg a@(FriendsInHighPlacesHenrysInformation attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      alejandroVela <- getSetAsideCard Assets.alejandroVela
      selectOne (locationIs Locations.townHall) >>= \case
        Just townHall -> createAssetAt_ alejandroVela (AttachedToLocation townHall)
        Nothing -> do
          townHall <- placeLocation =<< genCard Locations.townHall
          createAssetAt_ alejandroVela (AttachedToLocation townHall)
      advanceToAct attrs Acts.alejandrosPrison C
      pure a
    _ -> FriendsInHighPlacesHenrysInformation <$> liftRunMessage msg attrs
