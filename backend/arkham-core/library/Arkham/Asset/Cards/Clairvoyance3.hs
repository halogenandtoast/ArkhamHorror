module Arkham.Asset.Cards.Clairvoyance3 (
  clairvoyance3,
  clairvoyance3Effect,
  Clairvoyance3 (..),
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

newtype Clairvoyance3 = Clairvoyance3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance3 :: AssetCard Clairvoyance3
clairvoyance3 = asset Clairvoyance3 Cards.clairvoyance3

instance HasAbilities Clairvoyance3 where
  getAbilities (Clairvoyance3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Investigate)
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1]
    ]

instance RunMessage Clairvoyance3 where
  runMessage msg a@(Clairvoyance3 attrs) = case msg of
    UseCardAbility iid source@(isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ createCardEffect
            Cards.clairvoyance3
            Nothing
            source
            (InvestigationTarget iid lid)
        , skillTestModifiers attrs iid [DiscoveredClues 1, SkillModifier SkillWillpower 2]
        , Investigate
            iid
            lid
            source
            Nothing
            (if skillType == SkillIntellect then SkillWillpower else skillType)
            False
        ]
      pure a
    _ -> Clairvoyance3 <$> runMessage msg attrs

newtype Clairvoyance3Effect = Clairvoyance3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance3Effect :: EffectArgs -> Clairvoyance3Effect
clairvoyance3Effect = cardEffect Clairvoyance3Effect Cards.clairvoyance3

instance RunMessage Clairvoyance3Effect where
  runMessage msg e@(Clairvoyance3Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (chaosTokenFace token `elem` [ElderSign, PlusOne, Zero])
        $ pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token effectId)
              [InvestigatorAssignDamage iid effectSource DamageAny 0 1]
          , DisableEffect effectId
          ]
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> Clairvoyance3Effect <$> runMessage msg attrs
