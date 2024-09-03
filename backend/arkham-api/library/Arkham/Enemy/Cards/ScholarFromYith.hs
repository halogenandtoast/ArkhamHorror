module Arkham.Enemy.Cards.ScholarFromYith (scholarFromYith, ScholarFromYith (..)) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype ScholarFromYith = ScholarFromYith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scholarFromYith :: EnemyCard ScholarFromYith
scholarFromYith =
  enemyWith
    ScholarFromYith
    Cards.scholarFromYith
    (2, Static 2, 2)
    (0, 1)
    (preyL .~ Prey MostCardsInHand)

instance HasAbilities ScholarFromYith where
  getAbilities (ScholarFromYith a) =
    withBaseAbilities
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #when You AnyEnemyAttack (be a)
      , skillTestAbility
          $ restrictedAbility a 2 (exists $ EnemyIsEngagedWith You <> ReadyEnemy) parleyAction_
      ]

instance RunMessage ScholarFromYith where
  runMessage msg e@(ScholarFromYith attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ toMessage $ randomDiscard iid (attrs.ability 1)
        , toMessage $ randomDiscard iid (attrs.ability 1)
        ]
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      push $ parley sid iid (attrs.ability 2) iid #intellect (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      let drawing = drawCards iid (attrs.ability 2) 1
      pushAll [drawing, Msg.EnemyEvaded iid (toId attrs)]
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ toMessage $ enemyAttack (toId attrs) (attrs.ability 2) iid
      pure e
    _ -> ScholarFromYith <$> runMessage msg attrs
