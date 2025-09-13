module Arkham.Enemy.Cards.KnightOfTheOuterVoid (knightOfTheOuterVoid) where

import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.ForMovement
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.SkillType

newtype KnightOfTheOuterVoid = KnightOfTheOuterVoid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightOfTheOuterVoid :: EnemyCard KnightOfTheOuterVoid
knightOfTheOuterVoid =
  enemyWith
    KnightOfTheOuterVoid
    Cards.knightOfTheOuterVoid
    (3, Static 3, 4)
    (1, 1)
    (spawnAtL ?~ SpawnAt (ConnectedLocation NotForMovement))

instance HasAbilities KnightOfTheOuterVoid where
  getAbilities (KnightOfTheOuterVoid attrs) =
    withBaseAbilities
      attrs
      [ skillTestAbility
          $ restrictedAbility attrs 1 OnSameLocation
          $ ActionAbility [Action.Parley] (ActionCost 1)
      ]

instance RunMessage KnightOfTheOuterVoid where
  runMessage msg e@(KnightOfTheOuterVoid attrs) = case msg of
    Revelation _ (isSource attrs -> True) -> do
      lead <- getLeadPlayer
      canPlaceDoom <- toId attrs <=~> NotEnemy (EnemyWithModifier CannotPlaceDoomOnThis)
      when canPlaceDoom $ do
        push
          $ chooseOne
            lead
            [ Label "Place 1 doom" [PlaceDoom (toSource attrs) (toTarget attrs) 1]
            , Label "Place 2 doom" [PlaceDoom (toSource attrs) (toTarget attrs) 2]
            ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ Label "Use {willpower}" [parley sid iid (toAbilitySource attrs 1) attrs SkillWillpower (Fixed 4)]
          , Label "Use {intellect}" [parley sid iid (toAbilitySource attrs 1) attrs SkillIntellect (Fixed 4)]
          ]
      pure e
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      when (enemyDoom attrs > 0) $ do
        pushAll
          [ RemoveDoom (toAbilitySource attrs 1) (toTarget attrs) 1
          , PlaceClues (toAbilitySource attrs 1) (toTarget iid) 1
          ]
      pure e
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    _ -> KnightOfTheOuterVoid <$> runMessage msg attrs
