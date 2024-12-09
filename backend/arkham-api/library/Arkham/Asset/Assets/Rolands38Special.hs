module Arkham.Asset.Assets.Rolands38Special (rolands38Special) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype Rolands38Special = Rolands38Special AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rolands38Special :: AssetCard Rolands38Special
rolands38Special = asset Rolands38Special Cards.rolands38Special

instance HasAbilities Rolands38Special where
  getAbilities (Rolands38Special x) = [restricted x 1 ControlsThis $ fightAction $ assetUseCost x Ammo 1]

instance RunMessage Rolands38Special where
  runMessage msg a@(Rolands38Special attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      anyClues <- selectAny $ locationWithInvestigator iid <> LocationWithAnyClues
      let n = if anyClues then 3 else 1
      sid <- getRandom
      chooseFightEnemyWithModifiers sid iid (attrs.ability 1) [DamageDealt 1, SkillModifier #combat n]
      pure a
    _ -> Rolands38Special <$> liftRunMessage msg attrs
