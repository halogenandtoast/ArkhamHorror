module Arkham.Asset.Cards.Flashlight3 (flashlight3, Flashlight3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype Flashlight3 = Flashlight3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flashlight3 :: AssetCard Flashlight3
flashlight3 = asset Flashlight3 Cards.flashlight3

instance HasAbilities Flashlight3 where
  getAbilities (Flashlight3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (InitiatedSkillTest #when You #any #any (oneOf [#investigating, #evading]))
          (assetUseCost a Supply 1)
    ]

instance RunMessage Flashlight3 where
  runMessage msg a@(Flashlight3 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
      pure a
    _ -> Flashlight3 <$> liftRunMessage msg attrs
