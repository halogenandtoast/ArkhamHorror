module Arkham.Asset.Assets.EyeOfChaos (eyeOfChaos, eyeOfChaosEffect, EyeOfChaos (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (getSkillTestId)
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
  getAbilities (EyeOfChaos a) = [restricted a 1 ControlsThis $ investigateAction $ assetUseCost a Charge 1]

instance RunMessage EyeOfChaos where
  runMessage msg a@(EyeOfChaos attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      createCardEffect Cards.eyeOfChaos (effectMetaTarget sid) source iid
      aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    _ -> EyeOfChaos <$> liftRunMessage msg attrs

newtype EyeOfChaosEffect = EyeOfChaosEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfChaosEffect :: EffectArgs -> EyeOfChaosEffect
eyeOfChaosEffect = cardEffect EyeOfChaosEffect Cards.eyeOfChaos

instance RunMessage EyeOfChaosEffect where
  runMessage msg e@(EyeOfChaosEffect attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token -> do
      void $ runMaybeT do
        iid <- hoistMaybe attrs.target.investigator
        SkillTestTarget sid <- hoistMaybe attrs.metaTarget
        current <- MaybeT getSkillTestId
        guard $ sid == current
        lift do
          let
            handleIt assetId = do
              when (token.face == #curse) do
                lids <- select $ ConnectedLocation <> locationWithDiscoverableCluesBy iid
                stillInPlay <- selectAny $ AssetWithId assetId

                when (stillInPlay || notNull lids) do
                  chooseOrRunOneM iid do
                    when stillInPlay do
                      labeled "Place 1 Charge on Eye of Chaos" do
                        push $ AddUses attrs.source assetId Charge 1
                    labeled "Discover 1 clues at a connecting location" do
                      chooseTargetM iid lids \lid' -> discoverAt NotInvestigate iid attrs lid' 1
                disable attrs
          case attrs.source of
            AbilitySource (AssetSource assetId) 1 -> handleIt assetId
            AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            AbilitySource (IndexedSource _ (AssetSource assetId)) 1 -> handleIt assetId
            _ -> error "wrong source"
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    _ -> EyeOfChaosEffect <$> liftRunMessage msg attrs
