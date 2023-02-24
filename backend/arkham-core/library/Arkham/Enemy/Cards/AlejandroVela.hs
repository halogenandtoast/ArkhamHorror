module Arkham.Enemy.Cards.AlejandroVela
  ( alejandroVela
  , alejandroVelaEffect
  , AlejandroVela(..)
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
import Arkham.Token
import Arkham.Trait qualified as Trait

newtype AlejandroVela = AlejandroVela EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandroVela :: EnemyCard AlejandroVela
alejandroVela =
  enemy AlejandroVela Cards.alejandroVela (6, PerPlayer 4, 3) (1, 2)

instance HasAbilities AlejandroVela where
  getAbilities (AlejandroVela a) = withBaseAbilities
    a
    [ restrictedAbility a 1 OnSameLocation
      $ ActionAbility (Just Action.Parley)
      $ ActionCost 1
    ]

instance RunMessage AlejandroVela where
  runMessage msg e@(AlejandroVela attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ createCardEffect
          Cards.alejandroVela
          Nothing
          (toSource attrs)
          SkillTestTarget
        , parley
          iid
          (toSource attrs)
          (toTarget attrs)
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
                [CannotParleyWith $ enemyIs Enemies.alejandroVela]
              )
              (toSource attrs)
              (InvestigatorTarget iid)
            ]
          , Label
            "I accept"
            [ RemoveEnemy (toId attrs)
            , AdvanceToAct 1 Acts.timelock A (toSource attrs)
            , CreateWindowModifierEffect
              EffectGameWindow
              (EffectModifiers $ toModifiers
                attrs
                [ CannotParleyWith $ enemyIs Enemies.ichtacaScionOfYig
                , CannotBeAttackedBy $ EnemyWithTrait Trait.Cultist
                , CannotBeEngagedBy $ EnemyWithTrait Trait.Cultist
                ]
              )
              (toSource attrs)
              (InvestigatorTarget iid)
            ]
          ]
      pure e
    _ -> AlejandroVela <$> runMessage msg attrs

newtype AlejandroVelaEffect = AlejandroVelaEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandroVelaEffect :: EffectArgs -> AlejandroVelaEffect
alejandroVelaEffect =
  cardEffect AlejandroVelaEffect Cards.alejandroVela

instance HasModifiersFor AlejandroVelaEffect where
  getModifiersFor (TokenTarget token) (AlejandroVelaEffect a)
    | effectTarget a == SkillTestTarget && tokenFace token == Tablet
    = pure $ toModifiers a [ChangeTokenModifier AutoSuccessModifier]
  getModifiersFor _ _ = pure []

instance RunMessage AlejandroVelaEffect where
  runMessage msg e@(AlejandroVelaEffect attrs@EffectAttrs {..}) =
    case msg of
      SkillTestEnds _ _ | effectTarget == SkillTestTarget -> do
        push (DisableEffect effectId)
        pure e
      _ -> AlejandroVelaEffect <$> runMessage msg attrs
