module Arkham.Homebrew.AgesUnwound.Treacheries.PreserveCausality (preserveCausality) where

import Arkham.ChaosToken (ChaosTokenFace (ElderThing))
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (getActDecksInPlay)
import Arkham.Treachery.Import.Lifted

newtype PreserveCausality = PreserveCausality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preserveCausality :: TreacheryCard PreserveCausality
preserveCausality = treachery PreserveCausality Cards.preserveCausality

{- | "Peril. /
Revelation - If only one act deck is in play, Preserve Causality gains surge.
Otherwise, test [willpower] (3). For each point you fail by, take 1 damage. If
you fail by 3 or more, add 1 [elder_thing] token to the chaos bag for the
remainder of the campaign and remove Preserve Causality from the game."
-}
instance RunMessage PreserveCausality where
  runMessage msg t@(PreserveCausality attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      decks <- getActDecksInPlay
      if decks <= 1
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    {- "For each point you fail by, take 1 damage" is one assignment of N, not N
    separate ones: nothing in the loop makes its own choice, so it needs neither
    'doStep' nor a repeat. -}
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignDamage iid attrs n
      when (n >= 3) do
        addChaosToken ElderThing
        removeFromGame attrs
      pure t
    _ -> PreserveCausality <$> liftRunMessage msg attrs
