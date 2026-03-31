module Arkham.Asset.Assets.JimCulver4 (jimCulver4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype JimCulver4 = JimCulver4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jimCulver4 :: AssetCard JimCulver4
jimCulver4 = ally JimCulver4 Cards.jimCulver4 (3, 3)

instance HasModifiersFor JimCulver4 where
  getModifiersFor (JimCulver4 a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities JimCulver4 where
  getAbilities (JimCulver4 a) =
    [ controlled a 1 (youExist can.draw.cards)
        $ triggered
          (oneOf [InvestigatorTakeDamage #after You AnySource, InvestigatorTakeHorror #after You AnySource])
          (exhaust a)
    ]

instance RunMessage JimCulver4 where
  runMessage msg a@(JimCulver4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> JimCulver4 <$> liftRunMessage msg attrs
