module Arkham.Asset.Assets.JimCulverHauntedMusician (jimCulverHauntedMusician) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype JimCulverHauntedMusician = JimCulverHauntedMusician AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jimCulverHauntedMusician :: AssetCard JimCulverHauntedMusician
jimCulverHauntedMusician = ally JimCulverHauntedMusician Cards.jimCulverHauntedMusician (2, 2)

instance HasModifiersFor JimCulverHauntedMusician where
  getModifiersFor (JimCulverHauntedMusician a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities JimCulverHauntedMusician where
  getAbilities (JimCulverHauntedMusician a) =
    [ controlled a 1 (youExist can.draw.cards)
        $ triggered
          (oneOf [InvestigatorTakeDamage #after You AnySource, InvestigatorTakeHorror #after You AnySource])
          (exhaust a)
    ]

instance RunMessage JimCulverHauntedMusician where
  runMessage msg a@(JimCulverHauntedMusician attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> JimCulverHauntedMusician <$> liftRunMessage msg attrs
