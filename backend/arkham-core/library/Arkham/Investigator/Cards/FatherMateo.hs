module Arkham.Investigator.Cards.FatherMateo (
  fatherMateo,
  FatherMateo (..),
  fatherMateoElderSignEffect,
  FatherMateoElderSignEffect (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message hiding (RevealChaosToken)
import Arkham.Timing qualified as Timing
import Arkham.Window qualified as Window

newtype FatherMateo = FatherMateo InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fatherMateo :: InvestigatorCard FatherMateo
fatherMateo =
  investigator
    FatherMateo
    Cards.fatherMateo
    Stats
      { health = 6
      , sanity = 8
      , willpower = 4
      , intellect = 3
      , combat = 2
      , agility = 3
      }

instance HasAbilities FatherMateo where
  getAbilities (FatherMateo a) =
    [ limitedAbility (PlayerLimit PerGame 1) $
        restrictedAbility a 1 Self $
          ReactionAbility
            (RevealChaosToken Timing.After Anyone $ ChaosTokenFaceIs AutoFail)
            Free
    ]

instance HasChaosTokenValue FatherMateo where
  getChaosTokenValue iid ElderSign (FatherMateo attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign NoModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage FatherMateo where
  runMessage msg i@(FatherMateo attrs) = case msg of
    ResolveChaosToken _drawnToken token iid
      | token == ElderSign && iid == toId attrs -> do
          pushAll
            [ CreateEffect
                "04004"
                Nothing
                (toSource attrs)
                (InvestigatorTarget iid)
            , PassSkillTest
            ]
          pure i
    UseCardAbility _ source 1 (Window.revealedChaosTokens -> [token]) _
      | isSource attrs source -> do
          push $
            CreateChaosTokenEffect
              (EffectModifiers $ toModifiers attrs [ChaosTokenFaceModifier [ElderSign]])
              source
              token
          pure i
    _ -> FatherMateo <$> runMessage msg attrs

newtype FatherMateoElderSignEffect = FatherMateoElderSignEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fatherMateoElderSignEffect :: EffectArgs -> FatherMateoElderSignEffect
fatherMateoElderSignEffect =
  FatherMateoElderSignEffect . uncurry4 (baseAttrs "04004")

instance RunMessage FatherMateoElderSignEffect where
  runMessage msg e@(FatherMateoElderSignEffect attrs@EffectAttrs {..}) =
    case msg of
      SkillTestEnds _ _ ->
        e <$ case effectTarget of
          InvestigatorTarget iid -> do
            isTurn <- iid <=~> TurnInvestigator
            drawing <- drawCards iid attrs 1
            pushAll
              [ chooseOrRunOne iid $
                  Label
                    "Draw 1 card and gain 1 resource"
                    [drawing, TakeResources iid 1 (ChaosTokenEffectSource ElderSign) False]
                    : [ Label
                        "Take an additional action this turn"
                        [GainActions iid (toSource attrs) 1]
                      | isTurn
                      ]
              , DisableEffect effectId
              ]
          _ -> push (DisableEffect effectId)
      _ -> FatherMateoElderSignEffect <$> runMessage msg attrs
