module Arkham.Asset.Assets.Pickpocketing2 (pickpocketing2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
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
    [controlled_ a 1 $ triggered (Matcher.EnemyEvaded #after You AnyEnemy) (exhaust a)]

instance RunMessage Pickpocketing2 where
  runMessage msg a@(Pickpocketing2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      doBoth <- runMaybeT do
        guardM inEvasionSkillTest
        SucceededBy _ n <- MaybeT getSkillTestResultWithResultModifiers
        pure $ n >= 2
      drawOk <- can.draw.cards iid
      resourceOk <- can.gain.resources iid
      if fromMaybe False doBoth
        then chooseOneAtATimeM iid $ withI18n $ countVar 1 do
          when drawOk $ labeled' "drawCards" $ drawCards iid (attrs.ability 1) 1
          when resourceOk $ labeled' "gainResources" $ gainResources iid (attrs.ability 1) 1
        else chooseOneM iid $ withI18n $ countVar 1 do
          labeledValidate' drawOk "drawCards" $ drawCards iid (attrs.ability 1) 1
          labeledValidate' resourceOk "gainResources" $ gainResources iid (attrs.ability 1) 1
      pure a
    _ -> Pickpocketing2 <$> liftRunMessage msg attrs
