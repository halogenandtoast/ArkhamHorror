module Arkham.Asset.Cards.EighteenDerringer2 (eighteenDerringer2, EighteenDerringer2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Metadata = Metadata {givesBonus :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EighteenDerringer2 = EighteenDerringer2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eighteenDerringer2 :: AssetCard EighteenDerringer2
eighteenDerringer2 = asset (EighteenDerringer2 . (`with` Metadata False)) Cards.eighteenDerringer2

instance HasAbilities EighteenDerringer2 where
  getAbilities (EighteenDerringer2 (attrs `With` _)) =
    [restrictedAbility attrs 1 ControlsThis $ fightAction $ assetUseCost attrs Ammo 1]

instance RunMessage EighteenDerringer2 where
  runMessage msg (EighteenDerringer2 (attrs `With` metadata)) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      let amount = if givesBonus metadata then 3 else 2
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers attrs iid [DamageDealt 1, SkillModifier #combat amount], chooseFight]
      pure . EighteenDerringer2 $ attrs `with` Metadata False
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      push $ AddUses (toId attrs) Ammo 1
      pure . EighteenDerringer2 $ attrs `with` Metadata True
    EndRound -> pure . EighteenDerringer2 $ attrs `with` Metadata False
    _ -> EighteenDerringer2 . (`with` metadata) <$> runMessage msg attrs
