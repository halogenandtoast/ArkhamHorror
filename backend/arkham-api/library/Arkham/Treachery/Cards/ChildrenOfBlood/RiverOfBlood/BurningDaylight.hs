module Arkham.Treachery.Cards.ChildrenOfBlood.RiverOfBlood.BurningDaylight (burningDaylight) where

import Arkham.Agenda.Sequence (AgendaSide (A))
import Arkham.Matcher
import Arkham.Trait (Trait (Monster))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BurningDaylight = BurningDaylight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burningDaylight :: TreacheryCard BurningDaylight
burningDaylight = treachery BurningDaylight Cards.burningDaylight

instance RunMessage BurningDaylight where
  runMessage msg t@(BurningDaylight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      onFirstOrThirdAgenda <- selectAny $ AgendaWithSide A <> mapOneOf AgendaWithStep [1, 3]
      if onFirstOrThirdAgenda
        then placeDoomOnAgendaAndCheckAdvance 1
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      monsters <- getSetAsideCardsMatching (#enemy <> CardWithTrait Monster)
      for_ (nonEmpty monsters) $ drawCard iid <=< sample
      pure t
    _ -> BurningDaylight <$> liftRunMessage msg attrs
