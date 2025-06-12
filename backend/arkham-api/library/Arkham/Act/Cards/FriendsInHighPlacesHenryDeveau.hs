module Arkham.Act.Cards.FriendsInHighPlacesHenryDeveau (friendsInHighPlacesHenryDeveau) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (AssetCard)
import Arkham.Placement
import Arkham.Projection

newtype FriendsInHighPlacesHenryDeveau = FriendsInHighPlacesHenryDeveau ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

friendsInHighPlacesHenryDeveau :: ActCard FriendsInHighPlacesHenryDeveau
friendsInHighPlacesHenryDeveau =
  act (2, C) FriendsInHighPlacesHenryDeveau Cards.friendsInHighPlacesHenryDeveau Nothing

instance HasAbilities FriendsInHighPlacesHenryDeveau where
  getAbilities = actAbilities1' C \a ->
    restricted a 1 (exists $ assetIs Assets.henryDeveau <> AssetWithClues (AtLeast $ PerPlayer 1))
      $ Objective
      $ forced AnyWindow

instance RunMessage FriendsInHighPlacesHenryDeveau where
  runMessage msg a@(FriendsInHighPlacesHenryDeveau attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      henryDeveau <- selectJust $ assetIs Assets.henryDeveau
      henrysLocation <- selectJust $ locationWithAsset henryDeveau
      cardId <- field AssetCardId henryDeveau
      let henryDeveauAlejandrosKidnapper = lookupCard Enemies.henryDeveauAlejandrosKidnapper cardId
      createEnemyAt_ henryDeveauAlejandrosKidnapper henrysLocation
      push $ Flipped (toSource henryDeveau) henryDeveauAlejandrosKidnapper
      doStep 1 msg
      advanceToAct attrs Acts.alejandrosPlight C
      pure a
    DoStep 1 (AdvanceAct (isSide D attrs -> True) _ _) -> do
      alejandroVela <- getSetAsideCard Assets.alejandroVela
      henryDeveau <- selectJust $ enemyIs Enemies.henryDeveauAlejandrosKidnapper
      createAssetAt_ alejandroVela (AttachedToEnemy henryDeveau)
      pure a
    _ -> FriendsInHighPlacesHenryDeveau <$> liftRunMessage msg attrs
