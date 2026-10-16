{- | "Revelation - Test {willpower} (3). If you fail, take 1 horror and place 1
of your clues on your location. If you fail and there is a /Sorcerer/ enemy at
your location, resolve the fail effect an additional time."
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Treacheries.EnthrallingInterference (
  enthrallingInterference,
) where

import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Sorcerer))
import Arkham.Treachery.Import.Lifted

newtype EnthrallingInterference = EnthrallingInterference TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enthrallingInterference :: TreacheryCard EnthrallingInterference
enthrallingInterference = treachery EnthrallingInterference Cards.enthrallingInterference

instance RunMessage EnthrallingInterference where
  runMessage msg t@(EnthrallingInterference attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      sorcerer <- selectAny $ enemyAtLocationWith iid <> EnemyWithTrait Sorcerer
      repeated (if sorcerer then 2 else 1) do
        assignHorror iid attrs 1
        placeCluesOnLocation iid attrs 1
      pure t
    _ -> EnthrallingInterference <$> liftRunMessage msg attrs
