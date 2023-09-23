module Arkham.Enemy.Cards.KnightOfTheInnerCircle (
  knightOfTheInnerCircle,
  KnightOfTheInnerCircle (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype KnightOfTheInnerCircle = KnightOfTheInnerCircle EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightOfTheInnerCircle :: EnemyCard KnightOfTheInnerCircle
knightOfTheInnerCircle =
  enemyWith
    KnightOfTheInnerCircle
    Cards.knightOfTheInnerCircle
    (4, Static 4, 2)
    (2, 0)
    ((spawnAtL ?~ SpawnLocation ConnectedLocation) . (preyL .~ Prey MostKeys))

instance HasAbilities KnightOfTheInnerCircle where
  getAbilities (KnightOfTheInnerCircle attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ OrWindowMatcher
            [ Enters Timing.After You $ locationWithEnemy $ toId attrs
            , EnemyEnters Timing.After YourLocation $ EnemyWithId $ toId attrs
            ]
      ]

instance RunMessage KnightOfTheInnerCircle where
  runMessage msg e@(KnightOfTheInnerCircle attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid attrs iid SkillAgility 4
      pure e
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ EnemyEngageInvestigator (toId attrs) iid
      pure e
    _ -> KnightOfTheInnerCircle <$> runMessage msg attrs
