module Arkham.Homebrew.AgesUnwound.Treacheries.TheChainOfAforgomon (theChainOfAforgomon) where

import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheChainOfAforgomon = TheChainOfAforgomon TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChainOfAforgomon :: TreacheryCard TheChainOfAforgomon
theChainOfAforgomon = treachery TheChainOfAforgomon Cards.theChainOfAforgomon

{- | "__Revelation__ - Test [agility] (4). If you fail, place 1 doom on your
location, then take 1 damage for each point you failed by."

The damage is one assignment of X rather than X assignments: nothing in the
sentence is resolved per point except the amount, so no 'doStep' countdown is
needed.
-}
instance RunMessage TheChainOfAforgomon where
  runMessage msg t@(TheChainOfAforgomon attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      lid <- getJustLocation iid
      placeDoom attrs lid 1
      assignDamage iid attrs n
      pure t
    _ -> TheChainOfAforgomon <$> liftRunMessage msg attrs
