module Arkham.Asset.Assets.EyeOfChaos (eyeOfChaos, eyeOfChaosEffect) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.ForMovement
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.I18n
import Arkham.Investigate
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Token

newtype EyeOfChaos = EyeOfChaos AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfChaos :: AssetCard EyeOfChaos
eyeOfChaos = asset EyeOfChaos Cards.eyeOfChaos

instance HasAbilities EyeOfChaos where
  getAbilities (EyeOfChaos a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage EyeOfChaos where
  runMessage msg a@(EyeOfChaos attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      createSkillTestCardEffect sid Cards.eyeOfChaos Nothing source iid
      aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    _ -> EyeOfChaos <$> liftRunMessage msg attrs

newtype EyeOfChaosEffect = EyeOfChaosEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfChaosEffect :: EffectArgs -> EyeOfChaosEffect
eyeOfChaosEffect = cardEffect EyeOfChaosEffect Cards.eyeOfChaos

instance RunMessage EyeOfChaosEffect where
  runMessage msg (EyeOfChaosEffect attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token -> do
      fired <- runMaybeT do
        guard $ not attrs.finished
        guard $ token.face == #curse
        iid <- hoistMaybe attrs.target.investigator
        sid <- hoistMaybe attrs.skillTest
        current <- MaybeT getSkillTestId
        guard $ sid == current
        lift do
          let
            handleIt assetId = do
              lids <- select $ ConnectedLocation NotForMovement <> locationWithDiscoverableCluesBy iid
              stillInPlay <- selectAny $ AssetWithId assetId

              when (stillInPlay || notNull lids) do
                chooseOrRunOneM iid do
                  when stillInPlay do
                    cardI18n $ scope "eyeOfChaos" $ labeled "placeCharge" do
                      addUses attrs.source assetId Charge 1
                  unless (null lids) do
                    withI18n $ countVar 1 $ labeled "discoverCluesAtConnecting" do
                      chooseTargetM iid lids $ discoverAt NotInvestigate iid attrs 1
          case attrs.source of
            AbilitySource (AssetSource assetId) 1 -> handleIt assetId
            AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            AbilitySource (IndexedSource _ (AssetSource assetId)) 1 -> handleIt assetId
            UseAbilitySource _ (AssetSource assetId) 1 -> handleIt assetId
            UseAbilitySource _ (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            UseAbilitySource _ (IndexedSource _ (AssetSource assetId)) 1 -> handleIt assetId
            _ -> error "wrong source"
      pure $ EyeOfChaosEffect $ if isJust fired then finishedEffect attrs else attrs
    RepeatSkillTest _ stId
      | Just stId == attrs.skillTest ->
          EyeOfChaosEffect <$> liftRunMessage msg (unfinishedEffect attrs)
    _ -> EyeOfChaosEffect <$> liftRunMessage msg attrs
