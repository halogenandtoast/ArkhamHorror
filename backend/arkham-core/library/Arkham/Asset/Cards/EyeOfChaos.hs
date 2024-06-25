module Arkham.Asset.Cards.EyeOfChaos (eyeOfChaos, eyeOfChaosEffect, EyeOfChaos (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Effect.Runner
import Arkham.Investigate
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype EyeOfChaos = EyeOfChaos AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfChaos :: AssetCard EyeOfChaos
eyeOfChaos = asset EyeOfChaos Cards.eyeOfChaos

instance HasAbilities EyeOfChaos where
  getAbilities (EyeOfChaos a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#investigate] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage EyeOfChaos where
  runMessage msg a@(EyeOfChaos attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      investigation <- aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate iid source)
      pushAll $ createCardEffect Cards.eyeOfChaos Nothing source iid : leftOr investigation
      pure a
    _ -> EyeOfChaos <$> runMessage msg attrs

newtype EyeOfChaosEffect = EyeOfChaosEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfChaosEffect :: EffectArgs -> EyeOfChaosEffect
eyeOfChaosEffect = cardEffect EyeOfChaosEffect Cards.eyeOfChaos

instance RunMessage EyeOfChaosEffect where
  runMessage msg e@(EyeOfChaosEffect attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      let
        handleIt assetId = do
          when (token.face == #curse) do
            lids <- select $ ConnectedLocation <> LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
            stillInPlay <- selectAny $ AssetWithId assetId
            player <- getPlayer iid

            pushAll
              $ [ chooseOrRunOne
                  player
                  $ [Label "Place 1 Charge on Eye of Chaos" [AddUses assetId Charge 1] | stillInPlay]
                  <> [ Label
                        "Discover 1 clues at a connecting location"
                        [ chooseOne
                            player
                            [ targetLabel lid' [Msg.DiscoverClues iid $ discover lid' attrs 1]
                            | lid' <- lids
                            ]
                        ]
                     ]
                | stillInPlay || notNull lids
                ]
              <> [disable attrs]
      case attrs.source of
        AbilitySource (AssetSource assetId) 1 -> handleIt assetId
        AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
        _ -> error "wrong source"
      pure e
    SkillTestEnds _ _ -> do
      push (disable attrs)
      pure e
    _ -> EyeOfChaosEffect <$> runMessage msg attrs
