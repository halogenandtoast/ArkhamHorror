{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Burglary
  ( Burglary(..)
  , burglary
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype Burglary = Burglary Attrs
  deriving newtype (Show, ToJSON, FromJSON)

burglary :: AssetId -> Burglary
burglary uuid = Burglary $ baseAttrs uuid "01045"

instance HasModifiersFor env investigator Burglary where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Investigate))

instance IsInvestigator investigator => HasActions env investigator Burglary where
  getActions i NonFast (Burglary a) | ownedBy a i = pure
    [ ActivateCardAbilityAction (getId () i) (ability a)
    | not (assetExhausted a)
      && hasActionsRemaining i (Just Action.Investigate) (assetTraits a)
    ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Burglary where
  runMessage msg (Burglary attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      lid <- asks $ getId iid
      unshiftMessage $ Investigate
        iid
        lid
        SkillIntellect
        mempty
        mempty
        [TakeResources iid 3 False]
        False
      pure $ Burglary $ attrs & exhausted .~ True
    _ -> Burglary <$> runMessage msg attrs
