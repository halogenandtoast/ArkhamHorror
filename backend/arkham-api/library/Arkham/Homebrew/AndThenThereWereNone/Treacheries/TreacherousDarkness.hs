{- | "__Revelation__ - Test {agility} (X), where X is your location's shroud
value. If you fail, take 2 damage."
-}
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.TreacherousDarkness (
  treacherousDarkness,
) where

import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Treachery.Import.Lifted

newtype TreacherousDarkness = TreacherousDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousDarkness :: TreacheryCard TreacherousDarkness
treacherousDarkness = treachery TreacherousDarkness Cards.treacherousDarkness

instance RunMessage TreacherousDarkness where
  runMessage msg t@(TreacherousDarkness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility
        $ InvestigatorLocationMaybeFieldCalculation iid LocationShroud
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> TreacherousDarkness <$> liftRunMessage msg attrs
