module Arkham.Treachery.Cards.UnavoidableDemise (unavoidableDemise) where

import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnavoidableDemise = UnavoidableDemise TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unavoidableDemise :: TreacheryCard UnavoidableDemise
unavoidableDemise = treachery UnavoidableDemise Cards.unavoidableDemise

instance RunMessage UnavoidableDemise where
  runMessage msg t@(UnavoidableDemise attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility
        $ SumCalculation
          [ Fixed 2
          , CountTreacheries $ treacheryIs Cards.unavoidableDemise <> TreacheryWithPlacement NextToAgenda
          ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      n <- selectCount $ treacheryIs Cards.unavoidableDemise <> TreacheryWithPlacement NextToAgenda
      assignDamage iid attrs (2 + n)
      pure t
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    _ -> UnavoidableDemise <$> liftRunMessage msg attrs
