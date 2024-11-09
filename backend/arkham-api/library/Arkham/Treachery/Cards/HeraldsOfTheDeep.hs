module Arkham.Treachery.Cards.HeraldsOfTheDeep (heraldsOfTheDeep, HeraldsOfTheDeep (..)) where

import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HeraldsOfTheDeep = HeraldsOfTheDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heraldsOfTheDeep :: TreacheryCard HeraldsOfTheDeep
heraldsOfTheDeep = treachery HeraldsOfTheDeep Cards.heraldsOfTheDeep

instance RunMessage HeraldsOfTheDeep where
  runMessage msg t@(HeraldsOfTheDeep attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      flooded <- selectAny $ locationWithInvestigator iid <> FloodedLocation
      revelationSkillTest sid iid attrs #willpower (Fixed $ if flooded then 5 else 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      x <- getRemainingCurseTokens
      when (x < n) $ gainSurge attrs
      addCurseTokens (Just iid) (min x n)
      pure t
    _ -> HeraldsOfTheDeep <$> liftRunMessage msg attrs
