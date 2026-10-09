module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.WaltzOfTheSpheres (waltzOfTheSpheres) where

import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTowardsMatching)
import Arkham.Treachery.Import.Lifted

newtype WaltzOfTheSpheres = WaltzOfTheSpheres TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waltzOfTheSpheres :: TreacheryCard WaltzOfTheSpheres
waltzOfTheSpheres = treachery WaltzOfTheSpheres Cards.waltzOfTheSpheres

instance RunMessage WaltzOfTheSpheres where
  runMessage msg t@(WaltzOfTheSpheres attrs) = runQueueT $ case msg of
    {- "If you are at the location with the highest shroud value, Waltz of the
    Spheres gains surge. Otherwise, test agility (3)." -}
    Revelation iid (isSource attrs -> True) -> do
      atHighest <- selectAny $ HighestShroud Anywhere <> locationWithInvestigator iid
      if atHighest
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    -- "If you fail, move 1 location towards the location with the highest shroud value."
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      moveTowardsMatching (toSource attrs) iid (HighestShroud Anywhere)
      pure t
    _ -> WaltzOfTheSpheres <$> liftRunMessage msg attrs
