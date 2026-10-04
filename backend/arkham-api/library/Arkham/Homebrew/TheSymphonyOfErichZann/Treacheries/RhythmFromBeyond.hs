module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.RhythmFromBeyond (rhythmFromBeyond) where

import Arkham.Ability
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (placeMusicTreachery)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted hiding (SkillTestEnded)

newtype RhythmFromBeyond = RhythmFromBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rhythmFromBeyond :: TreacheryCard RhythmFromBeyond
rhythmFromBeyond = treachery RhythmFromBeyond Cards.rhythmFromBeyond

instance HasAbilities RhythmFromBeyond where
  -- "After you perform a skill test, if no cards were committed to this test:
  -- Take 1 horror."
  getAbilities (RhythmFromBeyond a) =
    [mkAbility a 1 $ forced $ SkillTestEnded #after Anyone (SkillTestWithCommittedCards NoCards)]

instance RunMessage RhythmFromBeyond where
  runMessage msg t@(RhythmFromBeyond attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeMusicTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    _ -> RhythmFromBeyond <$> liftRunMessage msg attrs
