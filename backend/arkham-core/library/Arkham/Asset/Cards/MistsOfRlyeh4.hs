module Arkham.Asset.Cards.MistsOfRlyeh4 (
  mistsOfRlyeh4,
  MistsOfRlyeh4 (..),
  mistsOfRlyeh4Effect,
  MistsOfRlyeh4Effect (..),
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

newtype MistsOfRlyeh4 = MistsOfRlyeh4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

mistsOfRlyeh4 :: AssetCard MistsOfRlyeh4
mistsOfRlyeh4 = asset MistsOfRlyeh4 Cards.mistsOfRlyeh4

instance HasAbilities MistsOfRlyeh4 where
  getAbilities (MistsOfRlyeh4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility [Action.Evade]
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Charge 1
    ]

instance RunMessage MistsOfRlyeh4 where
  runMessage msg a@(MistsOfRlyeh4 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ createCardEffect Cards.mistsOfRlyeh4 (Just $ EffectInt 1) source iid
        , createCardEffect Cards.mistsOfRlyeh4 (Just $ EffectInt 2) source iid
        , skillTestModifier source iid (SkillModifier SkillWillpower 3)
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
      pure a
    _ -> MistsOfRlyeh4 <$> runMessage msg attrs

newtype MistsOfRlyeh4Effect = MistsOfRlyeh4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

mistsOfRlyeh4Effect :: EffectArgs -> MistsOfRlyeh4Effect
mistsOfRlyeh4Effect = cardEffect MistsOfRlyeh4Effect Cards.mistsOfRlyeh4

instance RunMessage MistsOfRlyeh4Effect where
  runMessage msg e@(MistsOfRlyeh4Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | effectMetadata == Just (EffectInt 1) -> do
      when (effectTarget == InvestigatorTarget iid) $ do
        when
          ( chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
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
