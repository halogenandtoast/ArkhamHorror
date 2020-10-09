{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BeatCop where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import ClassyPrelude

newtype BeatCop = BeatCop Attrs
  deriving newtype (Show, ToJSON, FromJSON)

beatCop :: AssetId -> BeatCop
beatCop uuid = BeatCop $ (baseAttrs uuid "01018")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

instance IsInvestigator investigator => HasModifiersFor env investigator BeatCop where
  getModifiersFor _ i (BeatCop a) =
    pure [ SkillModifier SkillCombat 1 | ownedBy a i ]

instance IsInvestigator investigator => HasActions env investigator BeatCop where
  getActions i _ (BeatCop a) | ownedBy a i =
    pure [UseCardAbility (getId () i) (toSource a) Nothing 1]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env BeatCop where
  runMessage msg a@(BeatCop attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- asks $ getId @LocationId iid
      locationEnemyIds <- asks $ setToList . getSet locationId
      a <$ unshiftMessages
        [ Discard (toTarget attrs)
        , chooseOne
          iid
          [ EnemyDamage eid iid source 1 | eid <- locationEnemyIds ]
        ]
    _ -> BeatCop <$> runMessage msg attrs
