module Arkham.Enemy.Cards.LiarWithNoFace (
  liarWithNoFace,
  LiarWithNoFace (..),
)
where

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Types (Field (..))

newtype LiarWithNoFace = LiarWithNoFace EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liarWithNoFace :: EnemyCard LiarWithNoFace
liarWithNoFace =
  enemyWith
    LiarWithNoFace
    Cards.liarWithNoFace
    (3, Static 4, 3)
    (0, 2)
    (preyL .~ Prey MostCardsInHand)

instance HasAbilities LiarWithNoFace where
  getAbilities (LiarWithNoFace x) =
    withBaseAbilities
      x
      [ restrictedAbility
          x
          1
          (youExist $ InvestigatorWithTreacheryInHand $ TreacheryWithTitle "Whispering Chaos")
          $ forced
          $ EnemyAttacks #when You AnyEnemyAttack (be x)
      ]

instance RunMessage LiarWithNoFace where
  runMessage msg e@(LiarWithNoFace attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <-
        selectWithField TreacheryCard
          $ TreacheryInHandOf (InvestigatorWithId iid)
          <> TreacheryWithTitle "Whispering Chaos"
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel
            whisperingChaos
            [RevealCard (toCardId card), enemyAttackModifier (attrs.ability 1) attrs (DamageDealt 2)]
          | (whisperingChaos, card) <- cards
          ]
      pure e
    _ -> LiarWithNoFace <$> runMessage msg attrs
