module Arkham.Act.Cards.FriendsInHighPlacesHenryDeveau
  ( FriendsInHighPlacesHenryDeveau(..)
  , friendsInHighPlacesHenryDeveau
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Matcher hiding ( AssetCard )
import Arkham.Message
import Arkham.Placement
import Arkham.Source

newtype FriendsInHighPlacesHenryDeveau = FriendsInHighPlacesHenryDeveau ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

friendsInHighPlacesHenryDeveau :: ActCard FriendsInHighPlacesHenryDeveau
friendsInHighPlacesHenryDeveau = act
  (2, C)
  FriendsInHighPlacesHenryDeveau
  Cards.friendsInHighPlacesHenryDeveau
  Nothing

instance HasAbilities FriendsInHighPlacesHenryDeveau where
  getAbilities (FriendsInHighPlacesHenryDeveau a) =
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

instance RunMessage FriendsInHighPlacesHenryDeveau where
  runMessage msg a@(FriendsInHighPlacesHenryDeveau attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ 1 _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide D attrs -> do
      henryDeveau <- selectJust $ assetIs Assets.henryDeveau
      henrysLocation <- selectJust $ LocationWithAsset $ AssetWithId henryDeveau
      let
        henryDeveauAlejandrosKidnapper = EncounterCard $ lookupEncounterCard
          Enemies.henryDeveauAlejandrosKidnapper
          (unAssetId henryDeveau)
      pushAll
        [ CreateEnemyAt henryDeveauAlejandrosKidnapper henrysLocation Nothing
        , Flipped (AssetSource henryDeveau) henryDeveauAlejandrosKidnapper
        , NextAdvanceActStep aid 1
        , AdvanceToAct
          (actDeckId attrs)
          Acts.alejandrosPlight
          C
          (toSource attrs)
        ]
      pure a
    NextAdvanceActStep aid 1 | aid == actId attrs && onSide D attrs -> do
      alejandroVela <- getSetAsideCard Assets.alejandroVela
      henryDeveau <- selectJust $ enemyIs Enemies.henryDeveauAlejandrosKidnapper
      pushAll [CreateAssetAt alejandroVela (AttachedToEnemy henryDeveau)]
      pure a
    _ -> FriendsInHighPlacesHenryDeveau <$> runMessage msg attrs
