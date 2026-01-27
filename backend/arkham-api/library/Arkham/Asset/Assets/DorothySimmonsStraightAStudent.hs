module Arkham.Asset.Assets.DorothySimmonsStraightAStudent (dorothySimmonsStraightAStudent) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype DorothySimmonsStraightAStudent = DorothySimmonsStraightAStudent AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dorothySimmonsStraightAStudent :: AssetCard DorothySimmonsStraightAStudent
dorothySimmonsStraightAStudent = ally DorothySimmonsStraightAStudent Cards.dorothySimmonsStraightAStudent (1, 1)

instance HasModifiersFor DorothySimmonsStraightAStudent where
  getModifiersFor (DorothySimmonsStraightAStudent a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities DorothySimmonsStraightAStudent where
  getAbilities (DorothySimmonsStraightAStudent a) =
    [ controlled a 1 (youExist can.gain.resources)
        $ triggered
          (SkillTestResult #after You #investigation (SuccessResult $ oneOf [static 1, static 3]))
          (exhaust a)
    ]

instance RunMessage DorothySimmonsStraightAStudent where
  runMessage msg a@(DorothySimmonsStraightAStudent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> DorothySimmonsStraightAStudent <$> liftRunMessage msg attrs
