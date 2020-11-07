{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BeatCop where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype BeatCop = BeatCop Attrs
  deriving newtype (Show, ToJSON, FromJSON)

beatCop :: AssetId -> BeatCop
beatCop uuid = BeatCop $ baseAttrs uuid "01018" $ do
  slots .= [AllySlot]
  health ?= 2
  sanity ?= 2

instance HasModifiersFor env BeatCop where
  getModifiersFor _ (InvestigatorTarget iid) (BeatCop a) =
    pure [ SkillModifier SkillCombat 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env BeatCop where
  getActions iid _ (BeatCop a) | ownedBy a iid =
    pure [UseCardAbility iid (toSource a) Nothing 1]
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
