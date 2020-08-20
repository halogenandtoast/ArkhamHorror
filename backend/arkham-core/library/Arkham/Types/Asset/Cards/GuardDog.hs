{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.GuardDog where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude

newtype GuardDog = GuardDog Attrs
  deriving newtype (Show, ToJSON, FromJSON)

guardDog :: AssetId -> GuardDog
guardDog uuid = GuardDog $ (baseAttrs uuid "01021")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 1
  }

instance HasActions env investigator GuardDog where
  getActions i window (GuardDog x) = getActions i window x

instance (AssetRunner env) => RunMessage env GuardDog where
  runMessage msg (GuardDog attrs@Attrs {..}) = case msg of
    DidReceiveDamage (AssetTarget aid) (EnemySource eid) | aid == assetId -> do
      -- we must unshift the asset destroyed first before unshifting the question
      -- this is necessary to keep the asset as a valid investigator source of damage
      -- for any additional effects, such as triggering Roland's ability.
      result <- runMessage msg attrs
      unshiftMessage
        (Ask (getInvestigator attrs) $ ChooseOne
          [ EnemyDamage eid (getInvestigator attrs) (AssetSource aid) 1
          , Continue "Do not use Guard Dog's ability"
          ]
        )
      pure $ GuardDog result
    _ -> GuardDog <$> runMessage msg attrs
