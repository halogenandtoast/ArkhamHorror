module Arkham.Enemy.Cards.LiarWithNoFace (liarWithNoFace, LiarWithNoFace (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.Types (Field (..))

newtype LiarWithNoFace = LiarWithNoFace EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liarWithNoFace :: EnemyCard LiarWithNoFace
liarWithNoFace =
  enemyWith LiarWithNoFace Cards.liarWithNoFace (3, Static 4, 3) (0, 2)
    $ preyL
    .~ Prey MostCardsInHand

instance HasAbilities LiarWithNoFace where
  getAbilities (LiarWithNoFace x) =
    extend1 x
      $ restricted x 1 (youExist $ InvestigatorWithTreacheryInHand $ TreacheryWithTitle "Whispering Chaos")
      $ forced
      $ EnemyAttacks #when You AnyEnemyAttack (be x)

instance RunMessage LiarWithNoFace where
  runMessage msg e@(LiarWithNoFace attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <-
        selectWithField TreacheryCard $ treacheryInHandOf iid <> TreacheryWithTitle "Whispering Chaos"
      chooseOneM iid do
        for_ cards \(whisperingChaos, card) -> targeting whisperingChaos do
          push $ RevealCard card.id
          enemyAttackModifier (attrs.ability 1) attrs (DamageDealt 2)
      pure e
    _ -> LiarWithNoFace <$> liftRunMessage msg attrs
