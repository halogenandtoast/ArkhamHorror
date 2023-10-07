module Arkham.Asset.Cards.MistsOfRlyeh (
  mistsOfRlyeh,
  MistsOfRlyeh (..),
  mistsOfRlyehEffect,
  MistsOfRlyehEffect (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Location
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Movement
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Window qualified as Window

newtype MistsOfRlyeh = MistsOfRlyeh AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh :: AssetCard MistsOfRlyeh
mistsOfRlyeh = asset MistsOfRlyeh Cards.mistsOfRlyeh

instance HasAbilities MistsOfRlyeh where
  getAbilities (MistsOfRlyeh a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility
          (Just Action.Evade)
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage MistsOfRlyeh where
  runMessage msg a@(MistsOfRlyeh attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ createCardEffect Cards.mistsOfRlyeh (Just $ EffectInt 1) source iid
        , createCardEffect Cards.mistsOfRlyeh (Just $ EffectInt 2) source iid
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
      pure a
    _ -> MistsOfRlyeh <$> runMessage msg attrs

newtype MistsOfRlyehEffect = MistsOfRlyehEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyehEffect :: EffectArgs -> MistsOfRlyehEffect
mistsOfRlyehEffect = cardEffect MistsOfRlyehEffect Cards.mistsOfRlyeh

instance RunMessage MistsOfRlyehEffect where
  runMessage msg e@(MistsOfRlyehEffect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | effectMetadata == Just (EffectInt 1) -> do
      when (effectTarget == InvestigatorTarget iid) $ do
        when
          ( chaosTokenFace token
              `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
          )
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
              unblockedConnectedLocationIds <- accessibleLocations iid
              let
                moveOptions =
                  chooseOrRunOne iid
                    $ [Label "Do not move to a connecting location" []]
                    <> [ targetLabel lid [Move $ move attrs iid lid]
                       | lid <- unblockedConnectedLocationIds
                       ]
              pushAll [moveOptions, DisableEffect effectId]
            _ -> push $ DisableEffect effectId
        _ -> error "Invalid Target"
      pure e
    SkillTestEnds _ _ -> do
      push $ DisableEffect effectId
      pure e
    _ -> MistsOfRlyehEffect <$> runMessage msg attrs
