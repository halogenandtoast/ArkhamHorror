{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FirstAid where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype FirstAid = FirstAid Attrs
  deriving newtype (Show, ToJSON, FromJSON)

firstAid :: AssetId -> FirstAid
firstAid uuid = FirstAid $ baseAttrs uuid "01019"

instance (ActionRunner env investigator) => HasActions env investigator FirstAid where
  getActions i window (FirstAid Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      investigateAvailable <- hasInvestigateActions i window
      pure
        [ ActivateCardAbilityAction
            (getId () i)
            (mkAbility
              (AssetSource assetId)
              1
              (ActionAbility 1 (Just Action.Investigate))
            )
        | useCount assetUses > 0 && investigateAvailable
        ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env FirstAid where
  runMessage msg a@(FirstAid attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      FirstAid <$> runMessage msg (attrs & uses .~ Uses Resource.Supply 3)
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
      case assetUses of
        Uses Resource.Supply n -> do
          lid <- asks (getId @LocationId iid)
          investigatorIds <- HashSet.toList <$> asks (getSet lid)
          unshiftMessage
            (Ask
              iid
              (ChooseOne
                [ Ask
                    iid
                    (ChooseOne
                      [ HealDamage (InvestigatorTarget iid') 1
                      , HealHorror (InvestigatorTarget iid') 1
                      ]
                    )
                | iid' <- investigatorIds
                ]
              )
            )
          pure $ FirstAid $ attrs & uses .~ Uses Resource.Supply (n - 1)
        _ -> pure a
    _ -> FirstAid <$> runMessage msg attrs
