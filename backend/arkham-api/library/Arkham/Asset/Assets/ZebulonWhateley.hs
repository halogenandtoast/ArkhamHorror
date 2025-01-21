module Arkham.Asset.Assets.ZebulonWhateley (zebulonWhateley) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (controllerGets, ModifierType (..))
import Arkham.Matcher

newtype ZebulonWhateley = ZebulonWhateley AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zebulonWhateley :: AssetCard ZebulonWhateley
zebulonWhateley = ally ZebulonWhateley Cards.zebulonWhateley (1, 4)

instance HasAbilities ZebulonWhateley where
  getAbilities (ZebulonWhateley x) =
    [ restricted x 1 ControlsThis
        $ triggered
          (SkillTestResult #after You (SkillTestOnTreachery AnyTreachery) #success)
          (exhaust x)
    ]

instance HasModifiersFor ZebulonWhateley where
  getModifiersFor (ZebulonWhateley a) = controllerGets a [SkillModifier #willpower 1]

instance RunMessage ZebulonWhateley where
  runMessage msg a@(ZebulonWhateley attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> ZebulonWhateley <$> liftRunMessage msg attrs
