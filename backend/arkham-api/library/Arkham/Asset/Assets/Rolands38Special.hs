module Arkham.Asset.Assets.Rolands38Special (rolands38Special) where

import Arkham.Ability.Scripted.Builder
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (spendUses)
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Effect.Builder

newtype Rolands38Special = Rolands38Special AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasCardCode, Sourceable)
  deriving HasAbilities via Scripted Rolands38Special
  deriving RunMessage via Scripted Rolands38Special

rolands38Special :: AssetCard Rolands38Special
rolands38Special = asset Rolands38Special Cards.rolands38Special

instance ScriptedAbilities Rolands38Special where
  scriptedAbilities = abilities do
    fightAction do
      mustControl
      spendUses 1 Ammo
      fight $ effect you do
        damageDealt 1
        combat $ ifLocation (yourLocation <> withClues) (Fixed 3) (Fixed 1)
