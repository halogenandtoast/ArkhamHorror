module Arkham.Homebrew.AgesUnwound.Treacheries.GrandfatherParadox (grandfatherParadox) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (recordForInvestigator)
import Arkham.Trait (Trait (Past))
import Arkham.Treachery.Import.Lifted

newtype GrandfatherParadox = GrandfatherParadox TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandfatherParadox :: TreacheryCard GrandfatherParadox
grandfatherParadox = treachery GrandfatherParadox Cards.grandfatherParadox

{- | "__Revelation__ - Test [willpower] (3). If you are at a [[Past]] location,
this test gets +2 difficulty. If you fail, take 2 horror and remember that 'your
existence is waning.'"

"Remember" is a per-investigator campaign-log entry rather than a scenario note:
Resolution 2 records the names of each investigator whose existence is waning and
the epilogue reads them back with 'Arkham.Matcher.InvestigatorWithRecord', so the
entry has to be the log's.
-}
instance RunMessage GrandfatherParadox where
  runMessage msg t@(GrandfatherParadox attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      inThePast <- iid <=~> InvestigatorAt (LocationWithTrait Past)
      revelationSkillTest sid iid attrs #willpower (Fixed $ if inThePast then 5 else 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      recordForInvestigator iid ExistenceIsWaning
      pure t
    _ -> GrandfatherParadox <$> liftRunMessage msg attrs
