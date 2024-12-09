module Arkham.Asset.Assets.Armageddon (armageddon, armageddonEffect, Armageddon (..)) where

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

newtype Armageddon = Armageddon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddon :: AssetCard Armageddon
armageddon = asset Armageddon Cards.armageddon

instance HasAbilities Armageddon where
  getAbilities (Armageddon a) =
    [ restricted a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage Armageddon where
  runMessage msg a@(Armageddon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      skillTestModifier sid source iid (DamageDealt 1)
      createCardEffect Cards.armageddon (effectMetaTarget sid) source iid
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> Armageddon <$> liftRunMessage msg attrs

newtype ArmageddonEffect = ArmageddonEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddonEffect :: EffectArgs -> ArmageddonEffect
armageddonEffect = cardEffect ArmageddonEffect Cards.armageddon

instance RunMessage ArmageddonEffect where
  runMessage msg e@(ArmageddonEffect attrs) = runQueueT $ case msg of
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
                      labeled "Place 1 Charge on Armageddon" do
                        push $ AddUses attrs.source assetId Charge 1
                    when (notNull enemies) do
                      labeled "Deal 1 damage to an enemy at your location" do
                        chooseTargetM iid enemies $ nonAttackEnemyDamage attrs.source 1

                  disable attrs
          case attrs.source of
            AbilitySource (AssetSource assetId) 1 -> handleIt assetId
            AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            AbilitySource (IndexedSource _ (AssetSource assetId)) 1 -> handleIt assetId
            _ -> error "wrong source"
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    _ -> ArmageddonEffect <$> liftRunMessage msg attrs
