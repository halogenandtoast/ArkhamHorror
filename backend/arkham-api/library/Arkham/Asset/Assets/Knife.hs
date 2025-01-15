module Arkham.Asset.Assets.Knife (knife) where

import Arkham.Ability.Scripted.Builder
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Builder
import Arkham.Card

newtype Knife = Knife AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable, Sourceable, HasCardCode)
  deriving HasAbilities via Scripted Knife
  deriving RunMessage via Scripted Knife

knife :: AssetCard Knife
knife = asset Knife Cards.knife

instance ScriptedAbilities Knife where
  scriptedAbilities = abilities do
    fightAction do
      tooltip "{action}: _Fight_. You get +1 {combat} for this attack."
      fight $ effect you $ combat (Fixed 1)
    fightAction do
      tooltip "{action}: Discard Knife: _Fight_. You get +2 {combat} for this attack. This attack deals +1 damage."
      addCost $ discardCost this
      fight $ effect you $ combat (Fixed 2) >> damageDealt 1
