{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Burglary
  ( Burglary(..)
  , burglary
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Burglary = Burglary Attrs
  deriving newtype (Show, ToJSON, FromJSON)

burglary :: AssetId -> Burglary
burglary uuid = Burglary $ baseAttrs uuid "01045"

instance HasModifiersFor env Burglary where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Investigate))

instance ActionRunner env => HasActions env Burglary where
  getActions iid NonFast (Burglary a) | ownedBy a iid = do
    canAffordActions <- getCanAffordCost
      iid
      (toSource a)
      (ActionCost 1 (Just Action.Investigate) (assetTraits a))
    pure
      [ ActivateCardAbilityAction iid (ability a)
      | not (assetExhausted a) && canAffordActions
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Burglary where
  runMessage msg (Burglary attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      lid <- getId iid
      unshiftMessage
        $ CreateEffect "01045" Nothing source (InvestigationTarget iid lid)
      pure $ Burglary $ attrs & exhaustedL .~ True
    _ -> Burglary <$> runMessage msg attrs
