module Arkham.Act.Cards.FriendsInHighPlacesHenrysInformation
  ( FriendsInHighPlacesHenrysInformation(..)
  , friendsInHighPlacesHenrysInformation
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement

newtype FriendsInHighPlacesHenrysInformation = FriendsInHighPlacesHenrysInformation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

friendsInHighPlacesHenrysInformation
  :: ActCard FriendsInHighPlacesHenrysInformation
friendsInHighPlacesHenrysInformation = act
  (2, C)
  FriendsInHighPlacesHenrysInformation
  Cards.friendsInHighPlacesHenrysInformation
  Nothing

instance HasAbilities FriendsInHighPlacesHenrysInformation where
  getAbilities (FriendsInHighPlacesHenrysInformation a) =
    [ restrictedAbility
          a
          1
          (AssetExists $ assetIs Assets.henryDeveau <> AssetWithClues
            (AtLeast $ PerPlayer 1)
          )
        $ Objective
        $ ForcedAbility AnyWindow
    | onSide C a
    ]

instance RunMessage FriendsInHighPlacesHenrysInformation where
  runMessage msg a@(FriendsInHighPlacesHenrysInformation attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide D attrs -> do
      alejandroVela <- getSetAsideCard Assets.alejandroVela
      mTownHall <- selectOne $ locationIs Locations.townHall
      createAssetMessages <- case mTownHall of
        Just townHallId -> do
          assetId <- getRandom
          pure [CreateAssetAt assetId alejandroVela (AttachedToLocation townHallId)]
        Nothing -> do
          townHall <- genCard Locations.townHall
          (townHallId, placeTownHall) <- placeLocation townHall
          assetId <- getRandom
          pure [placeTownHall , CreateAssetAt assetId alejandroVela (AttachedToLocation townHallId)]

      pushAll
        $ createAssetMessages
        <> [AdvanceToAct (actDeckId attrs) Acts.alejandrosPrison C (toSource attrs)]
      pure a
    _ -> FriendsInHighPlacesHenrysInformation <$> runMessage msg attrs
