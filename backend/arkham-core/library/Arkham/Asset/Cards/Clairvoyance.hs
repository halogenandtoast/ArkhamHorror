module Arkham.Asset.Cards.Clairvoyance
  ( clairvoyance
  , clairvoyanceEffect
  , Clairvoyance(..)
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
import Arkham.Helpers.Investigator
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Token
import Arkham.Window qualified as Window

newtype Clairvoyance = Clairvoyance AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance :: AssetCard Clairvoyance
clairvoyance = asset Clairvoyance Cards.clairvoyance

instance HasAbilities Clairvoyance where
  getAbilities (Clairvoyance a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Investigate)
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1]
    ]

instance RunMessage Clairvoyance where
  runMessage msg a@(Clairvoyance attrs) = case msg of
    UseCardAbility iid source@(isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ createCardEffect Cards.clairvoyance Nothing source iid
        , skillTestModifier attrs iid (DiscoveredClues 1)
        , Investigate
          iid
          lid
          source
          Nothing
          (if skillType == SkillIntellect then SkillWillpower else skillType)
          False
        ]
      pure a
    _ -> Clairvoyance <$> runMessage msg attrs

newtype ClairvoyanceEffect = ClairvoyanceEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyanceEffect :: EffectArgs -> ClairvoyanceEffect
clairvoyanceEffect = cardEffect ClairvoyanceEffect Cards.clairvoyance

instance RunMessage ClairvoyanceEffect where
  runMessage msg e@(ClairvoyanceEffect attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (tokenFace token `elem` [ElderSign, PlusOne, Zero])
        (pushAll
          [ If
            (Window.RevealTokenEffect iid token effectId)
            [InvestigatorAssignDamage iid effectSource DamageAny 0 1]
          , DisableEffect effectId
          ]
        )
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> ClairvoyanceEffect <$> runMessage msg attrs
