module Arkham.Asset.Assets.Shrivelling (Shrivelling (..), shrivelling) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier

newtype Shrivelling = Shrivelling AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrivelling :: AssetCard Shrivelling
shrivelling = asset Shrivelling Cards.shrivelling

instance HasAbilities Shrivelling where
  getAbilities (Shrivelling a) =
    [ restricted a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage Shrivelling where
  runMessage msg a@(Shrivelling attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid source iid (DamageDealt 1)
      let tokens = [Skull, Cultist, Tablet, ElderThing, AutoFail]
      onRevealChaosTokenEffect sid (mapOneOf ChaosTokenFaceIs tokens) source sid do
        assignHorror iid source 1
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> Shrivelling <$> liftRunMessage msg attrs
