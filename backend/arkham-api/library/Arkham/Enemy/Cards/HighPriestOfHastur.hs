module Arkham.Enemy.Cards.HighPriestOfHastur (highPriestOfHastur) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Types (Field (..))

newtype HighPriestOfHastur = HighPriestOfHastur EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highPriestOfHastur :: EnemyCard HighPriestOfHastur
highPriestOfHastur =
  enemy HighPriestOfHastur Cards.highPriestOfHastur (6, Static 4, 2) (0, 0)
    & setPrey MostCardsInHand
    & setSpawnAt "Palace of the King"

instance HasAbilities HighPriestOfHastur where
  getAbilities (HighPriestOfHastur a) =
    extend1 a
      $ restricted a 1 (exists $ TreacheryWithTitle "Possession" <> TreacheryInHandOf You)
      $ forced
      $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage HighPriestOfHastur where
  runMessage msg e@(HighPriestOfHastur attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      possessions <- selectField TreacheryCard $ treacheryInHandOf iid <> TreacheryWithTitle "Possession"
      focusCards possessions $ chooseTargetM iid possessions revealCard
      drivenInsane iid
      investigatorDefeated (attrs.ability 1) iid
      pure e
    _ -> HighPriestOfHastur <$> liftRunMessage msg attrs
