module Arkham.Types.Asset.Cards.BeatCop
  ( BeatCop(..)
  , beatCop
  ) where

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
    pure $ toModifiers a [ SkillModifier SkillCombat 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability a =
  mkAbility (toSource a) 1 $ ActionAbility Nothing (DiscardCost $ toTarget a)

instance HasActions env BeatCop where
  getActions iid _ (BeatCop a) | ownedBy a iid =
    pure [ActivateCardAbilityAction iid (ability a)]
  getActions _ _ _ = pure []

-- | See: PlayerCardWithBehavior
instance AssetRunner env => RunMessage env BeatCop where
  runMessage msg (BeatCop attrs) = BeatCop <$> runMessage msg attrs
