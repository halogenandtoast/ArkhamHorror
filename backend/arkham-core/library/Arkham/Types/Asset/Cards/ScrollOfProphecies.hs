{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ScrollOfProphecies where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype ScrollOfProphecies = ScrollOfProphecies Attrs
  deriving newtype (Show, ToJSON, FromJSON)

scrollOfProphecies :: AssetId -> ScrollOfProphecies
scrollOfProphecies uuid =
  ScrollOfProphecies $ baseAttrs uuid "06116" $ slots .= [HandSlot]

instance HasModifiersFor env ScrollOfProphecies where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ScrollOfProphecies where
  getActions iid NonFast (ScrollOfProphecies a) | ownedBy a iid = do
    hasActionsRemaining <- getHasActionsRemaining
      iid
      Nothing
      (setToList $ assetTraits a)
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
      | not (assetExhausted a)
        && hasActionsRemaining
        && useCount (assetUses a)
        > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ScrollOfProphecies where
  runMessage msg (ScrollOfProphecies attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      ScrollOfProphecies
        <$> runMessage msg (attrs & uses .~ Uses Resource.Secret 4)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- asks $ getId @LocationId iid
      investigatorIds <- asks $ setToList . getSet locationId
      unshiftMessage
        (chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              [DrawCards iid' 3 False, ChooseAndDiscardCard iid']
          | iid' <- investigatorIds
          ]
        )
      pure
        $ ScrollOfProphecies
        $ attrs
        & exhausted
        .~ True
        & uses
        %~ Resource.use
    _ -> ScrollOfProphecies <$> runMessage msg attrs
