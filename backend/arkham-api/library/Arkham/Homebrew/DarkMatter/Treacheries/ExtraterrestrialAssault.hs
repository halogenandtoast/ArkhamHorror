module Arkham.Homebrew.DarkMatter.Treacheries.ExtraterrestrialAssault (extraterrestrialAssault) where

import Arkham.Card
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ExtraterrestrialAssault = ExtraterrestrialAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraterrestrialAssault :: TreacheryCard ExtraterrestrialAssault
extraterrestrialAssault = treachery ExtraterrestrialAssault Cards.extraterrestrialAssault

{- | "Revelation - Test [agility] (6). For each point you fail by, discard the top
card of the encounter deck. Draw each enemy discarded by this effect."
-}
instance RunMessage ExtraterrestrialAssault where
  runMessage msg t@(ExtraterrestrialAssault attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 6)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      discardTopOfEncounterDeckAndHandle iid attrs n attrs
      pure t
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      for_ (filter ((== EnemyType) . toCardType) cards) (drawCard iid)
      pure t
    _ -> ExtraterrestrialAssault <$> liftRunMessage msg attrs
