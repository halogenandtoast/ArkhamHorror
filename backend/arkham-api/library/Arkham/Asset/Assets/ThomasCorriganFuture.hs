module Arkham.Asset.Assets.ThomasCorriganFuture (thomasCorriganFuture) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype ThomasCorriganFuture = ThomasCorriganFuture AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasCorriganFuture :: AssetCard ThomasCorriganFuture
thomasCorriganFuture = ally ThomasCorriganFuture Cards.thomasCorriganFuture (1, 2)

instance HasModifiersFor ThomasCorriganFuture where
  getModifiersFor (ThomasCorriganFuture a) =
    modifySelect a (InvestigatorAt $ locationWithAsset a) [SkillModifier #willpower 1]

instance HasAbilities ThomasCorriganFuture where
  getAbilities (ThomasCorriganFuture a) =
    [ restricted a 1 OnSameLocation
        $ triggered (InitiatedSkillTest #when You AnySkillType AnySkillTestValue #any) (exhaust a)
    ]

instance RunMessage ThomasCorriganFuture where
  runMessage msg a@(ThomasCorriganFuture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        onSucceedByEffect sid AnyValue (attrs.ability 1) sid $ drawCards iid (attrs.ability 1) 1
      pure a
    _ -> ThomasCorriganFuture <$> liftRunMessage msg attrs
