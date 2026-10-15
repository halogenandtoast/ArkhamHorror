module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Treacheries.CollapsingRuin (collapsingRuin) where

import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Cursed, Ruined))
import Arkham.Treachery.Import.Lifted

newtype CollapsingRuin = CollapsingRuin TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collapsingRuin :: TreacheryCard CollapsingRuin
collapsingRuin = treachery CollapsingRuin Cards.collapsingRuin

instance RunMessage CollapsingRuin where
  runMessage msg t@(CollapsingRuin attrs) = runQueueT $ case msg of
    {- "Test [agility] (2). This test gets +1 difficulty if there is 2 or more
    damage on your location, and +1 difficulty if you are at a Ruined or Cursed
    location." Both clauses are independent and can both apply. -}
    Revelation iid (isSource attrs -> True) -> do
      let here = locationWithInvestigator iid
      damaged <- selectAny $ here <> LocationWithDamage (atLeast 2)
      unstable <- selectAny $ here <> oneOf [LocationWithTrait Ruined, LocationWithTrait Cursed]
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed $ 2 + count id [damaged, unstable])
      pure t
    -- "If you fail, take 1 damage. If you fail by 2 or more, also lose 1 action."
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignDamage iid attrs 1
      when (n >= 2) $ loseActions iid attrs 1
      pure t
    _ -> CollapsingRuin <$> liftRunMessage msg attrs
