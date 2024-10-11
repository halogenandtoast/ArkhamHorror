module Arkham.Asset.Assets.Shrivelling5 (Shrivelling5 (..), shrivelling5) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier

newtype Shrivelling5 = Shrivelling5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrivelling5 :: AssetCard Shrivelling5
shrivelling5 = asset Shrivelling5 Cards.shrivelling5

instance HasAbilities Shrivelling5 where
  getAbilities (Shrivelling5 a) = [restricted a 1 ControlsThis $ fightAction $ assetUseCost a Charge 1]

instance RunMessage Shrivelling5 where
  runMessage msg a@(Shrivelling5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #willpower 3, DamageDealt 2]
      let tokens = [Skull, Cultist, Tablet, ElderThing, AutoFail]
      onRevealChaosTokenEffect sid (mapOneOf ChaosTokenFaceIs tokens) source sid do
        assignHorror iid source 2
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> Shrivelling5 <$> liftRunMessage msg attrs
