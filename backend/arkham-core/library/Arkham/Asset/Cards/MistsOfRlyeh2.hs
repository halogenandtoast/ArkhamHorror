module Arkham.Asset.Cards.MistsOfRlyeh2
  ( mistsOfRlyeh2
  , MistsOfRlyeh2(..)
  , mistsOfRlyeh2Effect
  , MistsOfRlyeh2Effect(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher hiding ( MoveAction )
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Target
import Arkham.Token

newtype MistsOfRlyeh2 = MistsOfRlyeh2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh2 :: AssetCard MistsOfRlyeh2
mistsOfRlyeh2 = asset MistsOfRlyeh2 Cards.mistsOfRlyeh2

instance HasAbilities MistsOfRlyeh2 where
  getAbilities (MistsOfRlyeh2 a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Evade)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage MistsOfRlyeh2 where
  runMessage msg a@(MistsOfRlyeh2 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ pushAll
        [ createCardEffect
          Cards.mistsOfRlyeh2
          Nothing
          source
          (InvestigatorTarget iid)
        , skillTestModifier
          source
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 1)
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
    _ -> MistsOfRlyeh2 <$> runMessage msg attrs

newtype MistsOfRlyeh2Effect = MistsOfRlyeh2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh2Effect :: EffectArgs -> MistsOfRlyeh2Effect
mistsOfRlyeh2Effect = cardEffect MistsOfRlyeh2Effect Cards.mistsOfRlyeh2

instance RunMessage MistsOfRlyeh2Effect where
  runMessage msg e@(MistsOfRlyeh2Effect attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token -> case effectTarget of
      InvestigatorTarget iid' | iid == iid' -> e <$ when
        (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (push $ ChooseAndDiscardCard iid)
      _ -> pure e
    SkillTestEnds _ -> do
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
    _ -> MistsOfRlyeh2Effect <$> runMessage msg attrs
