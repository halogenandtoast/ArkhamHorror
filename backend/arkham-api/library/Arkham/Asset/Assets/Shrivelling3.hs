module Arkham.Asset.Assets.Shrivelling3 (Shrivelling3 (..), shrivelling3) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier

newtype Shrivelling3 = Shrivelling3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrivelling3 :: AssetCard Shrivelling3
shrivelling3 = asset Shrivelling3 Cards.shrivelling3

instance HasAbilities Shrivelling3 where
  getAbilities (Shrivelling3 a) = [restricted a 1 ControlsThis $ fightAction $ assetUseCost a Charge 1]

instance RunMessage Shrivelling3 where
  runMessage msg a@(Shrivelling3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #willpower 2, DamageDealt 1]
      let tokens = [Skull, Cultist, Tablet, ElderThing, AutoFail]
      onRevealChaosTokenEffect sid (mapOneOf ChaosTokenFaceIs tokens) source sid do
        assignHorror iid source 1
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> Shrivelling3 <$> liftRunMessage msg attrs
