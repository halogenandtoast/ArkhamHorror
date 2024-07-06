module Arkham.Asset.Cards.EyeOfChaos4 (eyeOfChaos4, eyeOfChaos4Effect, EyeOfChaos4 (..)) where

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

newtype EyeOfChaos4 = EyeOfChaos4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfChaos4 :: AssetCard EyeOfChaos4
eyeOfChaos4 = asset EyeOfChaos4 Cards.eyeOfChaos4

instance HasAbilities EyeOfChaos4 where
  getAbilities (EyeOfChaos4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#investigate] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage EyeOfChaos4 where
  runMessage msg a@(EyeOfChaos4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      investigation <- aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate iid source)
      pushAll
        $ [ skillTestModifier source iid (SkillModifier #intellect 2)
          , createCardEffect Cards.eyeOfChaos4 Nothing source iid
          ]
        <> leftOr investigation
      pure a
    _ -> EyeOfChaos4 <$> runMessage msg attrs

newtype EyeOfChaos4Effect = EyeOfChaos4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfChaos4Effect :: EffectArgs -> EyeOfChaos4Effect
eyeOfChaos4Effect = cardEffect EyeOfChaos4Effect Cards.eyeOfChaos4

instance RunMessage EyeOfChaos4Effect where
  runMessage msg e@(EyeOfChaos4Effect attrs) = case msg of
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
                  $ [Label "Place 1 Charge on Eye of Chaos (4)" [AddUses attrs.source assetId Charge 1] | stillInPlay]
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
      case attrs.source of
        AbilitySource (AssetSource assetId) 1 -> handleIt assetId
        AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
        _ -> error "wrong source"
      pure e
    SkillTestEnds _ _ -> do
      push (disable attrs)
      pure e
    _ -> EyeOfChaos4Effect <$> runMessage msg attrs
