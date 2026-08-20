module Arkham.Treachery.Cards.TheDreamEaters.TheSearchForKadath.ZoogBurrow (zoogBurrow, ZoogBurrow (..)) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Trait (Trait (Zoog))
import Arkham.Treachery.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Cards
import Arkham.Treachery.Runner

newtype ZoogBurrow = ZoogBurrow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoogBurrow :: TreacheryCard ZoogBurrow
zoogBurrow = treachery ZoogBurrow Cards.zoogBurrow

instance RunMessage ZoogBurrow where
  runMessage msg t@(ZoogBurrow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      zoogs <-
        select $ NearestEnemyToFallback iid $ EnemyWithTrait Zoog <> SwarmingEnemy <> NotEnemy IsSwarm
      if null zoogs
        then push $ findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Zoog
        else do
          player <- getPlayer iid
          lead <- getLead
          push $ chooseOrRunOne player [targetLabel zoog [PlaceSwarmCards lead zoog n] | zoog <- zoogs]
      pure t
    _ -> ZoogBurrow <$> liftRunMessage msg attrs
