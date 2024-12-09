module Arkham.Asset.Assets.JazzMulligan (jazzMulligan, JazzMulligan (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait

newtype JazzMulligan = JazzMulligan AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jazzMulligan :: AssetCard JazzMulligan
jazzMulligan =
  allyWith JazzMulligan Cards.jazzMulligan (2, 2)
    $ (isStoryL .~ True)
    . (slotsL .~ mempty)

instance HasAbilities JazzMulligan where
  getAbilities (JazzMulligan x) =
    [skillTestAbility $ restrictedAbility x 1 (Uncontrolled <> OnSameLocation) parleyAction_]

instance HasModifiersFor JazzMulligan where
  getModifiersFor (JazzMulligan a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      active <- iid <=~> ActiveInvestigator
      modifySelectWhen a active UnrevealedLocation [TraitRestrictedModifier Miskatonic Blank]

instance RunMessage JazzMulligan where
  runMessage msg a@(JazzMulligan attrs@AssetAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- fieldMap InvestigatorLocation (fromJustNote "must be at a location") iid
      push $ PlaceAsset assetId $ AtLocation lid
      pure a
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      sid <- getRandom
      push $ parley sid iid (attrs.ability 1) (toTarget attrs) #intellect (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ TakeControlOfAsset iid assetId
      pure a
    _ -> JazzMulligan <$> runMessage msg attrs
