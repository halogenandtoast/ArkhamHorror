module Arkham.Asset.Assets.Pickpocketing2 (pickpocketing2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillTest
import Arkham.SkillTestResult

newtype Pickpocketing2 = Pickpocketing2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pickpocketing2 :: AssetCard Pickpocketing2
pickpocketing2 = asset Pickpocketing2 Cards.pickpocketing2

instance HasAbilities Pickpocketing2 where
  getAbilities (Pickpocketing2 a) =
    [restricted a 1 ControlsThis $ triggered (Matcher.EnemyEvaded #after You AnyEnemy) (exhaust a)]

instance RunMessage Pickpocketing2 where
  runMessage msg a@(Pickpocketing2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      doBoth <- runMaybeT do
        st <- MaybeT getSkillTest
        guardM inEvasionSkillTest
        SucceededBy _ n <- pure (skillTestResult st)
        pure $ n >= 2
      if fromMaybe False doBoth
        then do
          drawCards iid (attrs.ability 1) 1
          gainResources iid (attrs.ability 1) 1
        else chooseOneM iid do
          (withI18n $ countVar 1 $ labeled' "drawCards") $ drawCards iid (attrs.ability 1) 1
          (withI18n $ countVar 1 $ labeled' "gainResources") $ gainResources iid (attrs.ability 1) 1
      pure a
    _ -> Pickpocketing2 <$> liftRunMessage msg attrs
