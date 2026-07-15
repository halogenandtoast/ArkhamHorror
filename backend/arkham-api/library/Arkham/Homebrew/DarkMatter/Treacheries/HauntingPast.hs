module Arkham.Homebrew.DarkMatter.Treacheries.HauntingPast (hauntingPast) where

import Arkham.Homebrew.DarkMatter.Helpers (getMemories)
import Arkham.Keyword (Keyword (Hidden))
import Arkham.Matcher
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HauntingPast = HauntingPast TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hauntingPast :: TreacheryCard HauntingPast
hauntingPast = treachery HauntingPast Cards.hauntingPast

instance RunMessage HauntingPast where
  runMessage msg t@(HauntingPast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      memories <- getMemories iid
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed memories)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      findAndDrawEncounterCard iid (CardWithKeyword Hidden)
      pure t
    _ -> HauntingPast <$> liftRunMessage msg attrs
