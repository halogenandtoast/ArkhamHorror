module Arkham.Types.Asset.Cards.HiredMuscle1
  ( hiredMuscle1
  , HiredMuscle1(..)
  )
where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target
import Arkham.Types.Asset.Attrs
import Arkham.Types.Game.Helpers

newtype HiredMuscle1 = HiredMuscle1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiredMuscle1 :: AssetId -> HiredMuscle1
hiredMuscle1 uuid = HiredMuscle1 $ (baseAttrs uuid "02027")
  { assetSlots = [AllySlot]
  , assetHealth = Just 3
  , assetSanity = Just 1
  }

instance HasActions env HiredMuscle1 where
  getActions iid window (HiredMuscle1 attrs) = getActions iid window attrs

instance HasModifiersFor env HiredMuscle1 where
  getModifiersFor _ (InvestigatorTarget iid) (HiredMuscle1 a) =
    pure [ toModifier a (SkillModifier SkillCombat 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env HiredMuscle1 where
  runMessage msg a@(HiredMuscle1 attrs@AssetAttrs {..}) = case msg of
    EndUpkeep -> do
      let iid = fromJustNote "must be owned" assetInvestigator
      a <$ unshiftMessage
        (chooseOne
          iid
          [ Label "Pay 1 Resource to Hired Muscle" [SpendResources iid 1]
          , Label "Discard Hired Muscle" [Discard $ toTarget attrs]
          ]
        )
    _ -> HiredMuscle1 <$> runMessage msg attrs
