module Arkham.Asset.Assets.EyeOfChaos4 (eyeOfChaos4, eyeOfChaos4Effect, EyeOfChaos4 (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Investigate
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype EyeOfChaos4 = EyeOfChaos4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfChaos4 :: AssetCard EyeOfChaos4
eyeOfChaos4 = asset EyeOfChaos4 Cards.eyeOfChaos4

instance HasAbilities EyeOfChaos4 where
  getAbilities (EyeOfChaos4 a) =
    [ restricted a 1 ControlsThis
        $ ActionAbilityWithSkill [#investigate] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage EyeOfChaos4 where
  runMessage msg a@(EyeOfChaos4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #willpower 2, DiscoveredClues 1]
      createCardEffect Cards.eyeOfChaos4 (effectMetaTarget sid) source iid
      aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    _ -> EyeOfChaos4 <$> liftRunMessage msg attrs

newtype EyeOfChaos4Effect = EyeOfChaos4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfChaos4Effect :: EffectArgs -> EyeOfChaos4Effect
eyeOfChaos4Effect = cardEffect EyeOfChaos4Effect Cards.eyeOfChaos4

instance RunMessage EyeOfChaos4Effect where
  runMessage msg e@(EyeOfChaos4Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token -> do
      void $ runMaybeT do
        guard $ isTarget iid attrs.target
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
                      labeled "Place 1 Charge on Eye of Chaos (4)" do
                        push $ AddUses attrs.source assetId Charge 1
                    labeled "Discover 1 clues at a connecting location" do
                      chooseTargetM iid lids \lid' -> discoverAt NotInvestigate iid attrs lid' 1
          case attrs.source of
            AbilitySource (AssetSource assetId) 1 -> handleIt assetId
            AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            AbilitySource (IndexedSource _ (AssetSource assetId)) 1 -> handleIt assetId
            _ -> error "wrong source"
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    _ -> EyeOfChaos4Effect <$> liftRunMessage msg attrs
