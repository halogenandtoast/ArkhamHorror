{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ClarityOfMind
  ( clarityOfMind
  , ClarityOfMind(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype ClarityOfMind = ClarityOfMind Attrs
  deriving newtype (Show, ToJSON, FromJSON)

clarityOfMind :: AssetId -> ClarityOfMind
clarityOfMind uuid =
  ClarityOfMind $ (baseAttrs uuid "02030") { assetSlots = [ArcaneSlot] }

instance HasActions env ClarityOfMind where
  getActions iid NonFast (ClarityOfMind a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
    | useCount (assetUses a) > 0
    ]
  getActions _ _ _ = pure []

instance HasModifiersFor env ClarityOfMind where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env ClarityOfMind where
  runMessage msg (ClarityOfMind attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      ClarityOfMind <$> runMessage msg (attrs & usesL .~ Uses Resource.Charge 3)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      lid <- getId @LocationId iid
      iids <- getSetList @InvestigatorId lid
      unshiftMessage
        (chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              [HealHorror (InvestigatorTarget iid') 1]
          | iid' <- iids
          ]
        )
      pure $ ClarityOfMind $ attrs & usesL %~ Resource.use
    _ -> ClarityOfMind <$> runMessage msg attrs
