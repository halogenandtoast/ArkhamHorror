module Arkham.Treachery.Cards.TheYellowSign where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Source
import Arkham.Strategy
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheYellowSign = TheYellowSign TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theYellowSign :: TreacheryCard TheYellowSign
theYellowSign = treachery TheYellowSign Cards.theYellowSign

instance RunMessage TheYellowSign where
  runMessage msg t@(TheYellowSign attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      search iid attrs iid [fromDeck] (basic $ withTrait Madness <> BasicWeaknessCard) (DrawFound iid 1)
      pure t
    _ -> TheYellowSign <$> liftRunMessage msg attrs
