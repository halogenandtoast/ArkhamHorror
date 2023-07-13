module Arkham.Asset.Cards.Clairvoyance5 (
  clairvoyance5,
  clairvoyance5Effect,
  Clairvoyance5 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Projection
import Arkham.SkillType
import Arkham.Window qualified as Window

newtype Clairvoyance5 = Clairvoyance5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance5 :: AssetCard Clairvoyance5
clairvoyance5 = asset Clairvoyance5 Cards.clairvoyance5

instance HasAbilities Clairvoyance5 where
  getAbilities (Clairvoyance5 a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Investigate) $
          Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1]
    ]

instance RunMessage Clairvoyance5 where
  runMessage msg a@(Clairvoyance5 attrs) = case msg of
    UseCardAbility iid source@(isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ createCardEffect
            Cards.clairvoyance5
            Nothing
            source
            (InvestigationTarget iid lid)
        , skillTestModifiers attrs iid [DiscoveredClues 2, SkillModifier SkillWillpower 3]
        , Investigate
            iid
            lid
            source
            Nothing
            (if skillType == SkillIntellect then SkillWillpower else skillType)
            False
        ]
      pure a
    _ -> Clairvoyance5 <$> runMessage msg attrs

newtype Clairvoyance5Effect = Clairvoyance5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance5Effect :: EffectArgs -> Clairvoyance5Effect
clairvoyance5Effect = cardEffect Clairvoyance5Effect Cards.clairvoyance5

instance RunMessage Clairvoyance5Effect where
  runMessage msg e@(Clairvoyance5Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (chaosTokenFace token `elem` [ElderSign, PlusOne, Zero])
        $ pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token effectId)
              [InvestigatorAssignDamage iid effectSource DamageAny 0 2]
          , DisableEffect effectId
          ]
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> Clairvoyance5Effect <$> runMessage msg attrs
