{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Asset.Cards.RiteOfSeeking
  ( riteOfSeeking
  , RiteOfSeeking(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype RiteOfSeeking = RiteOfSeeking Attrs
  deriving newtype (Show, ToJSON, FromJSON)

riteOfSeeking :: AssetId -> RiteOfSeeking
riteOfSeeking uuid =
  RiteOfSeeking $ (baseAttrs uuid "02028") { assetSlots = [ArcaneSlot] }

instance ActionRunner env => HasActions env RiteOfSeeking where
  getActions iid window (RiteOfSeeking a) | ownedBy a iid = do
    investigateAvailable <- hasInvestigateActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility (Just Action.Investigate) (ActionCost 1))
          )
      | useCount (assetUses a) > 0 && investigateAvailable
      ]
  getActions _ _ _ = pure []

instance HasModifiersFor env RiteOfSeeking where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env (), HasId LocationId env InvestigatorId) => RunMessage env RiteOfSeeking where
  runMessage msg (RiteOfSeeking attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      RiteOfSeeking <$> runMessage msg (attrs & usesL .~ Uses Resource.Charge 3)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      lid <- getId @LocationId iid
      unshiftMessages
        [ CreateEffect "02028" Nothing source (InvestigationTarget iid lid)
        , Investigate iid lid source SkillWillpower False
        ]
      pure $ RiteOfSeeking $ attrs & usesL %~ Resource.use
    _ -> RiteOfSeeking <$> runMessage msg attrs
