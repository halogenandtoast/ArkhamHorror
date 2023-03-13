module Arkham.Asset.Cards.MistsOfRlyeh4
  ( mistsOfRlyeh4
  , MistsOfRlyeh4(..)
  , mistsOfRlyeh4Effect
  , MistsOfRlyeh4Effect(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Discard
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher hiding ( MoveAction )
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Token
import Arkham.Window qualified as Window

newtype MistsOfRlyeh4 = MistsOfRlyeh4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh4 :: AssetCard MistsOfRlyeh4
mistsOfRlyeh4 = asset MistsOfRlyeh4 Cards.mistsOfRlyeh4

instance HasAbilities MistsOfRlyeh4 where
  getAbilities (MistsOfRlyeh4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Evade)
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Charge 1
    ]

instance RunMessage MistsOfRlyeh4 where
  runMessage msg a@(MistsOfRlyeh4 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ pushAll
        [ createCardEffect
          Cards.mistsOfRlyeh4
          (Just $ EffectInt 1)
          source
          (InvestigatorTarget iid)
        , createCardEffect
          Cards.mistsOfRlyeh4
          (Just $ EffectInt 2)
          source
          (InvestigatorTarget iid)
        , skillTestModifier
          source
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 3)
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
    _ -> MistsOfRlyeh4 <$> runMessage msg attrs

newtype MistsOfRlyeh4Effect = MistsOfRlyeh4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh4Effect :: EffectArgs -> MistsOfRlyeh4Effect
mistsOfRlyeh4Effect = cardEffect MistsOfRlyeh4Effect Cards.mistsOfRlyeh4

instance RunMessage MistsOfRlyeh4Effect where
  runMessage msg e@(MistsOfRlyeh4Effect attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | effectMetadata == Just (EffectInt 1) ->
      case effectTarget of
        InvestigatorTarget iid' | iid == iid' -> e <$ when
          (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
          )
          (pushAll
            [ If
              (Window.RevealTokenEffect iid token effectId)
              [toMessage $ chooseAndDiscardCard iid effectSource]
            , DisableEffect effectId
            ]
          )
        _ -> pure e
    SkillTestEnds _ _ | effectMetadata == Just (EffectInt 2) -> do
      case effectTarget of
        InvestigatorTarget iid -> do
          mSkillTestResult <- fmap skillTestResult <$> getSkillTest
          case mSkillTestResult of
            Just (SucceededBy _ _) -> do
              unblockedConnectedLocationIds <- selectList AccessibleLocation
              let
                moveOptions = chooseOrRunOne
                  iid
                  ([Label "Do not move to a connecting location" []]
                  <> [ targetLabel lid [MoveAction iid lid Free False]
                     | lid <- unblockedConnectedLocationIds
                     ]
                  )
              pushAll [moveOptions, DisableEffect effectId]
            _ -> push (DisableEffect effectId)
        _ -> error "Invalid Target"
      pure e
    _ -> MistsOfRlyeh4Effect <$> runMessage msg attrs
