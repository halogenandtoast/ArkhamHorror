module Arkham.Asset.Cards.MistsOfRlyeh4 (mistsOfRlyeh4, MistsOfRlyeh4 (..), mistsOfRlyeh4Effect) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Evade
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Movement
import Arkham.Prelude
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Window qualified as Window

newtype MistsOfRlyeh4 = MistsOfRlyeh4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh4 :: AssetCard MistsOfRlyeh4
mistsOfRlyeh4 = asset MistsOfRlyeh4 Cards.mistsOfRlyeh4

instance HasAbilities MistsOfRlyeh4 where
  getAbilities (MistsOfRlyeh4 a) = [restrictedAbility a 1 ControlsThis $ evadeAction $ assetUseCost a Charge 1]

instance RunMessage MistsOfRlyeh4 where
  runMessage msg a@(MistsOfRlyeh4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseEvade <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade iid source)
      pushAll
        $ [ createCardEffect Cards.mistsOfRlyeh4 (Just $ EffectInt 1) source iid
          , createCardEffect Cards.mistsOfRlyeh4 (Just $ EffectInt 2) source iid
          , skillTestModifier source iid (SkillModifier SkillWillpower 3)
          ]
        <> chooseEvade
      pure a
    _ -> MistsOfRlyeh4 <$> runMessage msg attrs

newtype MistsOfRlyeh4Effect = MistsOfRlyeh4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh4Effect :: EffectArgs -> MistsOfRlyeh4Effect
mistsOfRlyeh4Effect = cardEffect MistsOfRlyeh4Effect Cards.mistsOfRlyeh4

instance RunMessage MistsOfRlyeh4Effect where
  runMessage msg e@(MistsOfRlyeh4Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | effectMetadata == Just (EffectInt 1) -> do
      when (effectTarget == InvestigatorTarget iid) $ do
        when (token.face `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
          $ pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token effectId)
                [toMessage $ chooseAndDiscardCard iid effectSource]
            , DisableEffect effectId
            ]
      pure e
    SkillTestEnds _ _ | effectMetadata == Just (EffectInt 2) -> do
      case effectTarget of
        InvestigatorTarget iid -> do
          mSkillTestResult <- fmap skillTestResult <$> getSkillTest
          case mSkillTestResult of
            Just (SucceededBy _ _) -> do
              unblockedConnectedLocationIds <- getAccessibleLocations iid attrs
              player <- getPlayer iid
              let
                moveOptions =
                  chooseOrRunOne player
                    $ [Label "Do not move to a connecting location" []]
                    <> [ targetLabel lid [Move $ move attrs iid lid]
                       | lid <- unblockedConnectedLocationIds
                       ]
              pushAll [moveOptions, DisableEffect effectId]
            _ -> push (DisableEffect effectId)
        _ -> error "Invalid Target"
      pure e
    _ -> MistsOfRlyeh4Effect <$> runMessage msg attrs
