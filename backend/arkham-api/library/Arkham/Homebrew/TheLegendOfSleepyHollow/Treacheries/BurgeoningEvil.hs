{- | "Attach Burgeoning Evil to The Headless Horseman, even if he is out of
play." The three lines stack: the third copy leaves all three in force.
-}
module Arkham.Homebrew.TheLegendOfSleepyHollow.Treacheries.BurgeoningEvil (burgeoningEvil) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype BurgeoningEvil = BurgeoningEvil TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burgeoningEvil :: TreacheryCard BurgeoningEvil
burgeoningEvil = treachery BurgeoningEvil Cards.burgeoningEvil

{- | Only the lowest-id copy publishes the modifiers, so the count is applied
once however many copies are attached.
-}
instance HasModifiersFor BurgeoningEvil where
  getModifiersFor (BurgeoningEvil a) =
    selectOne (IncludeOutOfPlayEnemy headlessHorseman) >>= traverse_ \horseman -> do
      attached <-
        sort
          <$> select
            (treacheryIs Cards.burgeoningEvil <> TreacheryIsAttachedTo (toTarget horseman))
      when (take 1 attached == [a.id]) do
        let n = length attached
        modifySelect a headlessHorseman
          $ concat
            [ guard (n >= 1) *> [DamageDealt 1, EnemyFight 1]
            , guard (n >= 2) *> [HorrorDealt 1, EnemyEvade 1]
            , guard (n >= 3) *> [AddKeyword Keyword.Retaliate, AddKeyword Keyword.Alert]
            ]

instance RunMessage BurgeoningEvil where
  runMessage msg t@(BurgeoningEvil attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectOne (IncludeOutOfPlayEnemy headlessHorseman) >>= traverse_ (attachTreachery attrs)
      pure t
    _ -> BurgeoningEvil <$> liftRunMessage msg attrs
