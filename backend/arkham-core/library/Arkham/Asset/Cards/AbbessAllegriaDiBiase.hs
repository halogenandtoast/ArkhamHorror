module Arkham.Asset.Cards.AbbessAllegriaDiBiase
  ( abbessAllegriaDiBiase
  , AbbessAllegriaDiBiase(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Id
import Arkham.Matcher hiding (MoveAction)
import Arkham.Target

newtype AbbessAllegriaDiBiase = AbbessAllegriaDiBiase AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbessAllegriaDiBiase :: AssetCard AbbessAllegriaDiBiase
abbessAllegriaDiBiase =
  ally AbbessAllegriaDiBiase Cards.abbessAllegriaDiBiase (2, 2)

instance HasAbilities AbbessAllegriaDiBiase where
  getAbilities (AbbessAllegriaDiBiase attrs) = case assetLocation attrs of
    Just abbessLocation ->
      [ restrictedAbility
          attrs
          1
          (AnyCriterion
            [ LocationExists
              (LocationWithId abbessLocation <> AccessibleLocation)
            , LocationExists $ YourLocation <> AccessibleFrom
              (LocationWithId abbessLocation)
            ]
          )
          (FastAbility $ ExhaustCost (toTarget attrs))
      ]
    Nothing -> []

getAssetLocation
  :: (MonadReader env m, HasId LocationId env InvestigatorId)
  => AssetAttrs
  -> m LocationId
getAssetLocation AssetAttrs {..} = case assetLocation of
  Just location -> pure location
  Nothing -> case assetController of
    Just iid -> getId iid
    Nothing -> error "Invalid location for Abbess"

instance AssetRunner env => RunMessage env AbbessAllegriaDiBiase where
  runMessage msg a@(AbbessAllegriaDiBiase attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      investigatorLocationId <- getId @LocationId iid
      abbessLocationId <- getAssetLocation attrs
      a <$ if investigatorLocationId == abbessLocationId
        then do
          connectedLocationIds <- map unConnectedLocationId
            <$> getSetList abbessLocationId
          push
            (chooseOrRunOne
              iid
              [ TargetLabel
                  (LocationTarget connectedLocationId)
                  [MoveAction iid connectedLocationId Free False]
              | connectedLocationId <- connectedLocationIds
              ]
            )
        else push (MoveAction iid abbessLocationId Free False)
    _ -> AbbessAllegriaDiBiase <$> runMessage msg attrs
