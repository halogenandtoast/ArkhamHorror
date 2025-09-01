module Arkham.Asset.Assets.GuardianAngel (guardianAngel) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.ForMovement
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (getTotalDamage)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype GuardianAngel = GuardianAngel AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardianAngel :: AssetCard GuardianAngel
guardianAngel = assetWith GuardianAngel Cards.guardianAngel (healthL ?~ 3)

instance HasModifiersFor GuardianAngel where
  getModifiersFor (GuardianAngel a) = for_ a.controller \controller ->
    modifySelectMaybe a (not_ (InvestigatorWithId controller)) \iid -> do
      location <- MaybeT $ field AssetLocation (toId a)
      liftGuardM $ iid <=~> InvestigatorAt (orConnected NotForMovement location)
      pure [CanAssignDamageToAsset a.id]

instance HasAbilities GuardianAngel where
  getAbilities (GuardianAngel attrs) =
    [restricted attrs 1 ControlsThis $ freeReaction $ AssetDealtDamage #when AnySource (be attrs)]

instance RunMessage GuardianAngel where
  runMessage msg a@(GuardianAngel attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows' _ -> do
      let damage = getTotalDamage windows'
      n <- min damage <$> getRemainingBlessTokens
      pushAll $ replicate n $ AddChaosToken BlessToken
      pure a
    _ -> GuardianAngel <$> runMessage msg attrs
