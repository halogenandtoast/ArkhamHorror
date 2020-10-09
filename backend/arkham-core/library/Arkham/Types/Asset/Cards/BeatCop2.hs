{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BeatCop2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype BeatCop2 = BeatCop2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

beatCop2 :: AssetId -> BeatCop2
beatCop2 uuid = BeatCop2 $ (baseAttrs uuid "01018")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 2
  }

instance IsInvestigator investigator => HasModifiersFor env investigator BeatCop2 where
  getModifiersFor _ i (BeatCop2 a) =
    pure [ SkillModifier SkillCombat 1 | ownedBy a i ]

instance (IsInvestigator investigator) => HasActions env investigator BeatCop2 where
  getActions i _ (BeatCop2 a) | ownedBy a i =
    pure [UseCardAbility (getId () i) (toSource a) (toSource a) Nothing 1]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env BeatCop2 where
  runMessage msg a@(BeatCop2 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs ->
      a <$ unshiftMessage
        (AddModifiers
          (InvestigatorTarget iid)
          (toSource attrs)
          [SkillModifier SkillCombat 1]
        )
    UseCardAbility iid _ source _ 1 | isSource attrs source -> do
      locationId <- asks $ getId @LocationId (getInvestigator attrs)
      locationEnemyIds <- asks $ setToList . getSet locationId
      unshiftMessages
        [ AssetDamage (assetId attrs) (toSource attrs) 1 0
        , chooseOne
          iid
          [ EnemyDamage eid iid (toSource attrs) 1 | eid <- locationEnemyIds ]
        ]
      pure . BeatCop2 $ attrs & exhausted .~ True
    _ -> BeatCop2 <$> runMessage msg attrs
