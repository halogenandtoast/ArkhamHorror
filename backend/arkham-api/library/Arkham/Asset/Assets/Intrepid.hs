module Arkham.Asset.Assets.Intrepid (intrepid, Intrepid (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Intrepid = Intrepid AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intrepid :: AssetCard Intrepid
intrepid = asset Intrepid Cards.intrepid

instance HasModifiersFor Intrepid where
  getModifiersFor (Intrepid a) = controllerGets a $ map (`SkillModifier` 1) [#intellect, #combat, #agility]

instance HasAbilities Intrepid where
  getAbilities (Intrepid a) =
    [restrictedAbility a 1 ControlsThis $ ForcedAbility $ RoundEnds #when]

instance RunMessage Intrepid where
  runMessage msg a@(Intrepid attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure a
    _ -> Intrepid <$> runMessage msg attrs
