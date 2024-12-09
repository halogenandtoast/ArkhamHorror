module Arkham.Asset.Assets.Armageddon4 (armageddon4, armageddon4Effect, Armageddon4 (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Fight
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Armageddon4 = Armageddon4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddon4 :: AssetCard Armageddon4
armageddon4 = asset Armageddon4 Cards.armageddon4

instance HasAbilities Armageddon4 where
  getAbilities (Armageddon4 a) =
    [ restricted a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage Armageddon4 where
  runMessage msg a@(Armageddon4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #willpower 2]
      createCardEffect Cards.armageddon4 (effectMetaTarget sid) source iid
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> Armageddon4 <$> liftRunMessage msg attrs

newtype Armageddon4Effect = Armageddon4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddon4Effect :: EffectArgs -> Armageddon4Effect
armageddon4Effect = cardEffect Armageddon4Effect Cards.armageddon4

instance RunMessage Armageddon4Effect where
  runMessage msg e@(Armageddon4Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      withSkillTest \sid -> do
        when (maybe False (isTarget sid) attrs.metaTarget) $ do
          let
            handleIt assetId = do
              when (token.face == #curse) do
                enemies <- select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource attrs.source
                stillInPlay <- selectAny $ AssetWithId assetId
                when (stillInPlay || notNull enemies) do
                  chooseOrRunOneM iid do
                    when stillInPlay do
                      labeled "Place 1 Charge on Armageddon4" do
                        push $ AddUses attrs.source assetId Charge 1
                    when (notNull enemies) do
                      labeled "Deal 1 damage to an enemy at your location" do
                        chooseTargetM iid enemies $ nonAttackEnemyDamage attrs.source 1
          case attrs.source of
            AbilitySource (AssetSource assetId) 1 -> handleIt assetId
            AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            AbilitySource (IndexedSource _ (AssetSource assetId)) 1 -> handleIt assetId
            _ -> error "wrong source"
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    _ -> Armageddon4Effect <$> liftRunMessage msg attrs
