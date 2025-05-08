module Arkham.Treachery.Cards.BlackStarsRise (blackStarsRise) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BlackStarsRise = BlackStarsRise TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackStarsRise :: TreacheryCard BlackStarsRise
blackStarsRise = treachery BlackStarsRise Cards.blackStarsRise

instance RunMessage BlackStarsRise where
  runMessage msg t@(BlackStarsRise attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      hasAgenda <- selectAny AnyAgenda
      chooseOrRunOneM iid $ withI18n do
        when hasAgenda $ labeled' "placeAgendaDoomCanAdvance" $ placeDoomOnAgendaAndCheckAdvance 1
        chooseTakeHorror iid attrs n
      pure t
    _ -> BlackStarsRise <$> liftRunMessage msg attrs
