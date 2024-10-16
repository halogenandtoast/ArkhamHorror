module Arkham.Asset.Assets.Armageddon (armageddon, armageddonEffect, Armageddon (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Prelude

newtype Armageddon = Armageddon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddon :: AssetCard Armageddon
armageddon = asset Armageddon Cards.armageddon

instance HasAbilities Armageddon where
  getAbilities (Armageddon a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage Armageddon where
  runMessage msg a@(Armageddon attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      chooseFight <- aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      enabled <- skillTestModifier sid source iid (DamageDealt 1)
      pushAll
        $ [ enabled
          , createCardEffect Cards.armageddon (effectMetaTarget sid) source iid
          ]
        <> leftOr chooseFight
      pure a
    _ -> Armageddon <$> runMessage msg attrs

newtype ArmageddonEffect = ArmageddonEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddonEffect :: EffectArgs -> ArmageddonEffect
armageddonEffect = cardEffect ArmageddonEffect Cards.armageddon

instance RunMessage ArmageddonEffect where
  runMessage msg e@(ArmageddonEffect attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      withSkillTest \sid -> do
        when (maybe False (isTarget sid) attrs.metaTarget) $ do
          let
            handleIt assetId = do
              when (token.face == #curse) do
                enemies <- select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource attrs.source
                stillInPlay <- selectAny $ AssetWithId assetId
                player <- getPlayer iid
                pushAll
                  $ [ chooseOrRunOne
                      player
                      $ [ Label "Place 1 Charge on Armageddon" [disable attrs, AddUses attrs.source assetId Charge 1]
                        | stillInPlay
                        ]
                      <> [ Label
                            "Deal 1 damage to an enemy at your location"
                            [ chooseOne
                                player
                                [targetLabel enemy [disable attrs, EnemyDamage enemy $ nonAttack attrs.source 1] | enemy <- enemies]
                            ]
                         ]
                    | stillInPlay || notNull enemies
                    ]
          case attrs.source of
            AbilitySource (AssetSource assetId) 1 -> handleIt assetId
            AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            _ -> error "wrong source"
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      push (disable attrs)
      pure e
    _ -> ArmageddonEffect <$> runMessage msg attrs
