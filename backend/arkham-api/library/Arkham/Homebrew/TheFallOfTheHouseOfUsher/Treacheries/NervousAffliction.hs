module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Treacheries.NervousAffliction (nervousAffliction) where

import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype NervousAffliction = NervousAffliction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nervousAffliction :: TreacheryCard NervousAffliction
nervousAffliction = treachery NervousAffliction Cards.nervousAffliction

instance RunMessage NervousAffliction where
  runMessage msg t@(NervousAffliction attrs) = runQueueT $ case msg of
    {- "If you do not control any clues, Nervous Affliction gains surge.
    Otherwise, test [intellect] (X), where X is equal to the number of clues you
    control (max 4)." -}
    Revelation iid (isSource attrs -> True) -> do
      clues <- field InvestigatorClues iid
      if clues == 0
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #intellect (Fixed $ min 4 clues)
      pure t
    {- "If you fail, take 1 horror. If you fail by 2 or more, place 1 of your
    clues on your location." -}
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs 1
      when (n >= 2) $ placeCluesOnLocation iid attrs 1
      pure t
    _ -> NervousAffliction <$> liftRunMessage msg attrs
