module Arkham.Treachery.Cards.HauntingPastDarkMatter (hauntingPastDarkMatter) where

import Arkham.Campaigns.DarkMatter.Helpers (getMemories)
import Arkham.Keyword (Keyword (Hidden))
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HauntingPastDarkMatter = HauntingPastDarkMatter TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hauntingPastDarkMatter :: TreacheryCard HauntingPastDarkMatter
hauntingPastDarkMatter = treachery HauntingPastDarkMatter Cards.hauntingPastDarkMatter

instance RunMessage HauntingPastDarkMatter where
  runMessage msg t@(HauntingPastDarkMatter attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      memories <- getMemories iid
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed memories)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      findAndDrawEncounterCard iid (CardWithKeyword Hidden)
      pure t
    _ -> HauntingPastDarkMatter <$> liftRunMessage msg attrs
