module Arkham.Types.Asset.Cards.ScrollOfProphecies
  ( ScrollOfProphecies(..)
  , scrollOfProphecies
  ) where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype ScrollOfProphecies = ScrollOfProphecies AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfProphecies :: AssetId -> ScrollOfProphecies
scrollOfProphecies uuid =
  ScrollOfProphecies $ (baseAttrs uuid "06116") { assetSlots = [HandSlot] }

instance HasModifiersFor env ScrollOfProphecies where
  getModifiersFor = noModifiersFor

instance HasActions env ScrollOfProphecies where
  getActions iid NonFast (ScrollOfProphecies a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
    | not (assetExhausted a) && useCount (assetUses a) > 0
    ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env ScrollOfProphecies where
  runMessage msg (ScrollOfProphecies attrs@AssetAttrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      ScrollOfProphecies
        <$> runMessage msg (attrs & usesL .~ Uses Resource.Secret 4)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
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
        & exhaustedL
        .~ True
        & usesL
        %~ Resource.use
    _ -> ScrollOfProphecies <$> runMessage msg attrs
