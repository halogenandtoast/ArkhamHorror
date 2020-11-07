{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.GuardDog where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype GuardDog = GuardDog Attrs
  deriving newtype (Show, ToJSON, FromJSON)

guardDog :: AssetId -> GuardDog
guardDog uuid = GuardDog $ baseAttrs uuid "01021" $ do
  slots .= [AllySlot]
  health ?= 3
  sanity ?= 1

instance HasModifiersFor env GuardDog where
  getModifiersFor _ _ _ = pure []

instance HasActions env GuardDog where
  getActions i window (GuardDog x) = getActions i window x

instance (AssetRunner env) => RunMessage env GuardDog where
  runMessage msg (GuardDog attrs@Attrs {..}) = case msg of
    DidReceiveDamage (AssetTarget aid) (EnemySource eid) | aid == assetId -> do
      -- we must unshift the asset destroyed first before unshifting the question
      -- this is necessary to keep the asset as a valid investigator source of damage
      -- for any additional effects, such as triggering Roland's ability.
      result <- runMessage msg attrs
      unshiftMessage $ chooseOne
        (getInvestigator attrs)
        [ EnemyDamage eid (getInvestigator attrs) (AssetSource aid) 1
        , Continue "Do not use Guard Dog's ability"
        ]
      pure $ GuardDog result
    _ -> GuardDog <$> runMessage msg attrs
