module Arkham.Types.Asset.Cards.BeatCop where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype BeatCop = BeatCop Attrs
  deriving newtype (Show, ToJSON, FromJSON)

beatCop :: AssetId -> BeatCop
beatCop uuid = BeatCop $ (baseAttrs uuid "01018")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

instance HasModifiersFor env BeatCop where
  getModifiersFor _ (InvestigatorTarget iid) (BeatCop a) =
    pure [ toModifier a (SkillModifier SkillCombat 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env BeatCop where
  getActions iid _ (BeatCop a) | ownedBy a iid =
    pure [UseCardAbility iid (toSource a) Nothing 1]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env BeatCop where
  runMessage msg a@(BeatCop attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- getId @LocationId iid
      locationEnemyIds <- getSetList locationId
      a <$ unshiftMessages
        [ Discard (toTarget attrs)
        , chooseOne
          iid
          [ EnemyDamage eid iid source 1 | eid <- locationEnemyIds ]
        ]
    _ -> BeatCop <$> runMessage msg attrs
