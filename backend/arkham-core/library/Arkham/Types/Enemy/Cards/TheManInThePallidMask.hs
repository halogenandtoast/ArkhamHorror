module Arkham.Types.Enemy.Cards.TheManInThePallidMask
  ( theManInThePallidMask
  , TheManInThePallidMask(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype TheManInThePallidMask = TheManInThePallidMask EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theManInThePallidMask :: EnemyCard TheManInThePallidMask
theManInThePallidMask = enemyWith
  TheManInThePallidMask
  Cards.theManInThePallidMask
  (4, Static 3, 4)
  (0, 1)
  (spawnAtL ?~ FarthestLocationFromAll Anywhere)

instance HasAbilities TheManInThePallidMask where
  getAbilities (TheManInThePallidMask a) = withBaseAbilities
    a
    [ restrictedAbility a 1 OnSameLocation
      $ ActionAbility (Just Action.Investigate)
      $ ActionCost 1
    ]

instance EnemyRunner env => RunMessage env TheManInThePallidMask where
  runMessage msg e@(TheManInThePallidMask attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId iid
      e <$ pushAll
        [ skillTestModifier source (LocationTarget lid) (ShroudModifier 2)
        , Investigate
          iid
          lid
          source
          (Just $ toTarget attrs)
          SkillIntellect
          False
        ]
    Successful (Action.Investigate, _) iid _ target _ | isTarget attrs target ->
      e <$ push (DefeatEnemy (toId attrs) iid (toSource attrs))
    _ -> TheManInThePallidMask <$> runMessage msg attrs
