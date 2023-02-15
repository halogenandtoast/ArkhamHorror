module Arkham.Enemy.Cards.IchtacaScionOfYig
  ( ichtacaScionOfYig
  , ichtacaScionOfYigEffect
  , IchtacaScionOfYig(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence
import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Story.Cards qualified as Story
import Arkham.Target
import Arkham.Token
import Arkham.Trait (Trait(Serpent))

newtype IchtacaScionOfYig = IchtacaScionOfYig EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaScionOfYig :: EnemyCard IchtacaScionOfYig
ichtacaScionOfYig =
  enemy IchtacaScionOfYig Cards.ichtacaScionOfYig (4, PerPlayer 6, 4) (2, 1)

instance HasAbilities IchtacaScionOfYig where
  getAbilities (IchtacaScionOfYig a) = withBaseAbilities
    a
    [ restrictedAbility a 1 OnSameLocation
      $ ActionAbility (Just Action.Parley)
      $ ActionCost 1
    ]

instance RunMessage IchtacaScionOfYig where
  runMessage msg e@(IchtacaScionOfYig attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ createCardEffect
          Cards.ichtacaScionOfYig
          Nothing
          (toSource attrs)
          SkillTestTarget
        , beginSkillTest
          iid
          (toSource attrs)
          (toTarget attrs)
          (Just Action.Parley)
          SkillIntellect
          5
        ]
      pure e
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ InitiateEnemyAttack iid (toId attrs) RegularAttack
        pure e
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ Flip iid (toSource attrs) (toTarget attrs)
        pure e
    Flip iid _ target | isTarget attrs target -> do
      push $ ReadStory iid Story.yigsMercy
      pure e
    ResolveStory iid story' | story' == Story.yigsMercy -> do
      yigsFury <- getRecordCount YigsFury
      push $ chooseOne iid $ if yigsFury >= 16
        then [Label "Ichtaca refuses your plea" []]
        else
          [ Label
            "I could never turn my back on humanity"
            [ Exhaust (toTarget attrs)
            , DisengageEnemyFromAll (toId attrs)
            , CreateWindowModifierEffect
              EffectGameWindow
              (EffectModifiers $ toModifiers
                attrs
                [CannotParleyWith $ enemyIs Enemies.ichtacaScionOfYig]
              )
              (toSource attrs)
              (InvestigatorTarget iid)
            ]
          , Label
            "I accept"
            [ RemoveEnemy (toId attrs)
            , AdvanceToAct 1 Acts.paradiseLost A (toSource attrs)
            , CreateWindowModifierEffect
              EffectGameWindow
              (EffectModifiers $ toModifiers
                attrs
                [ CannotParleyWith $ enemyIs Enemies.alejandroVela
                , CannotBeAttackedBy $ EnemyWithTrait Serpent
                , CannotBeEngagedBy $ EnemyWithTrait Serpent
                ]
              )
              (toSource attrs)
              (InvestigatorTarget iid)
            ]
          ]
      pure e
    _ -> IchtacaScionOfYig <$> runMessage msg attrs

newtype IchtacaScionOfYigEffect = IchtacaScionOfYigEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaScionOfYigEffect :: EffectArgs -> IchtacaScionOfYigEffect
ichtacaScionOfYigEffect =
  cardEffect IchtacaScionOfYigEffect Cards.ichtacaScionOfYig

instance HasModifiersFor IchtacaScionOfYigEffect where
  getModifiersFor (TokenTarget token) (IchtacaScionOfYigEffect a)
    | effectTarget a == SkillTestTarget && tokenFace token == Cultist
    = pure $ toModifiers a [ChangeTokenModifier AutoSuccessModifier]
  getModifiersFor _ _ = pure []

instance RunMessage IchtacaScionOfYigEffect where
  runMessage msg e@(IchtacaScionOfYigEffect attrs@EffectAttrs {..}) =
    case msg of
      SkillTestEnds _ _ | effectTarget == SkillTestTarget -> do
        push (DisableEffect effectId)
        pure e
      _ -> IchtacaScionOfYigEffect <$> runMessage msg attrs
