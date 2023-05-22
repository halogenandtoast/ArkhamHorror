module Arkham.Asset.Cards.Wither4
  ( wither4
  , wither4Effect
  , Wither4(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.SkillType
import Arkham.Token
import Arkham.Window qualified as Window

newtype Wither4 = Wither4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wither4 :: AssetCard Wither4
wither4 = asset Wither4 Cards.wither4

instance HasAbilities Wither4 where
  getAbilities (Wither4 a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbilityWithSkill
        (Just Action.Fight)
        SkillWillpower
        (ActionCost 1)
    ]

instance RunMessage Wither4 where
  runMessage msg a@(Wither4 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
      [ createCardEffect Cards.wither4 Nothing source (InvestigatorTarget iid)
      , skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier SkillWillpower 2)
      , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
      ]
    _ -> Wither4 <$> runMessage msg attrs

newtype Wither4Effect = Wither4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wither4Effect :: EffectArgs -> Wither4Effect
wither4Effect = cardEffect Wither4Effect Cards.wither4

instance RunMessage Wither4Effect where
  runMessage msg e@(Wither4Effect attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      enemyId <- fromJustNote "no attacked enemy" <$> getAttackedEnemy
      when
        (tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing])
        (pushAll
          [ If
            (Window.RevealTokenEffect iid token effectId)
            [CreateWindowModifierEffect EffectTurnWindow (EffectModifiers $ toModifiers attrs [EnemyFightWithMin (-1) (Min 1), EnemyEvadeWithMin (-1) (Min 1), HealthModifierWithMin (-1) (Min 1)]) (AbilitySource effectSource 1) (toTarget enemyId)]
          , DisableEffect effectId
          ]
        )
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> Wither4Effect <$> runMessage msg attrs
