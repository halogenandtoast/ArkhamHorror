module Arkham.Asset.Cards.TrackShoes (
  trackShoes,
  TrackShoes (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (MoveAction)
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype TrackShoes = TrackShoes AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trackShoes :: AssetCard TrackShoes
trackShoes = asset TrackShoes Cards.trackShoes

instance HasModifiersFor TrackShoes where
  getModifiersFor (InvestigatorTarget iid) (TrackShoes attrs)
    | attrs `controlledBy` iid =
        pure $
          toModifiers attrs [SkillModifier SkillAgility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities TrackShoes where
  getAbilities (TrackShoes attrs) =
    [ restrictedAbility attrs 1 ControlsThis $
        ReactionAbility
          (MovedButBeforeEnemyEngagement Timing.After You Anywhere)
          (ExhaustCost $ toTarget attrs)
    ]

instance RunMessage TrackShoes where
  runMessage msg a@(TrackShoes attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ beginSkillTest iid source (InvestigatorTarget iid) SkillAgility 3
      pure a
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          accessibleLocationIds <- selectList AccessibleLocation
          push $
            chooseOne
              iid
              [ TargetLabel (LocationTarget lid) [MoveAction iid lid Free False]
              | lid <- accessibleLocationIds
              ]
          pure a
    _ -> TrackShoes <$> runMessage msg attrs
