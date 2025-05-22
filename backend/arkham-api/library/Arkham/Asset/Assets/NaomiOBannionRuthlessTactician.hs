module Arkham.Asset.Assets.NaomiOBannionRuthlessTactician (naomiOBannionRuthlessTactician) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, withSkillTest)
import Arkham.Helpers.Window
import Arkham.Matcher

newtype NaomiOBannionRuthlessTactician = NaomiOBannionRuthlessTactician AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

naomiOBannionRuthlessTactician :: AssetCard NaomiOBannionRuthlessTactician
naomiOBannionRuthlessTactician = ally NaomiOBannionRuthlessTactician Cards.naomiOBannionRuthlessTactician (3, 3)

instance HasModifiersFor NaomiOBannionRuthlessTactician where
  getModifiersFor (NaomiOBannionRuthlessTactician a) = controllerGets a [SkillModifier #intellect 1, SkillModifier #combat 1]

instance HasAbilities NaomiOBannionRuthlessTactician where
  getAbilities (NaomiOBannionRuthlessTactician a) =
    [ restricted
        a
        1
        (ControlsThis <> DuringSkillTest (mapOneOf SkillTestWithSkillType [#intellect, #combat]))
        $ triggered (RevealChaosToken #after (colocatedWithMatch You) (not_ #autofail)) (exhaust a)
    ]

instance RunMessage NaomiOBannionRuthlessTactician where
  runMessage msg a@(NaomiOBannionRuthlessTactician attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      cancelChaosToken (attrs.ability 1) iid token
      getSkillTestInvestigator >>= traverse_ \iid' -> do
        withSkillTest \sid ->
          skillTestModifier
            sid
            (attrs.ability 1)
            sid
            (CancelAnyChaosTokenAndDrawAnother $ ChaosTokenFaceIs token.face)
        drawAnotherChaosToken iid'
      pure a
    _ -> NaomiOBannionRuthlessTactician <$> liftRunMessage msg attrs
