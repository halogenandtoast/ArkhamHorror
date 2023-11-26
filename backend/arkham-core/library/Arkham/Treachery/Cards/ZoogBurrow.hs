module Arkham.Treachery.Cards.ZoogBurrow (
  zoogBurrow,
  ZoogBurrow (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait (Trait (Zoog))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ZoogBurrow = ZoogBurrow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoogBurrow :: TreacheryCard ZoogBurrow
zoogBurrow = treachery ZoogBurrow Cards.zoogBurrow

instance RunMessage ZoogBurrow where
  runMessage msg t@(ZoogBurrow attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #agility 3
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      zoogs <- selectList $ NearestEnemyTo iid $ EnemyWithTrait Zoog <> SwarmingEnemy <> NotEnemy IsSwarm
      if null zoogs
        then push $ findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Zoog
        else do
          player <- getPlayer iid
          lead <- getLead
          push $ chooseOrRunOne player [targetLabel zoog [PlaceSwarmCards lead zoog n] | zoog <- zoogs]
      pure t
    _ -> ZoogBurrow <$> runMessage msg attrs
