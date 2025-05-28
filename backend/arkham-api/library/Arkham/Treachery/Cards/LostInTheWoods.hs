module Arkham.Treachery.Cards.LostInTheWoods (lostInTheWoods) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LostInTheWoods = LostInTheWoods TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheWoods :: TreacheryCard LostInTheWoods
lostInTheWoods = treachery LostInTheWoods Cards.lostInTheWoods

instance RunMessage LostInTheWoods where
  runMessage msg t@(LostInTheWoods attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      selectEach (investigator_ $ at_ "Enchanted Woods") \iid' -> do
        sid <- getRandom
        revelationSkillTest sid iid' attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      loseActions iid attrs 1
      assignHorror iid attrs 1
      pure t
    _ -> LostInTheWoods <$> liftRunMessage msg attrs
