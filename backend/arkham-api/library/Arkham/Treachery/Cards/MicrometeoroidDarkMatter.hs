module Arkham.Treachery.Cards.MicrometeoroidDarkMatter (micrometeoroidDarkMatter) where

import Arkham.Campaigns.DarkMatter.Helpers (campaignI18n)
import Arkham.Card.CardType
import Arkham.Helpers.Message.Discard (discardAll)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MicrometeoroidDarkMatter = MicrometeoroidDarkMatter TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

micrometeoroidDarkMatter :: TreacheryCard MicrometeoroidDarkMatter
micrometeoroidDarkMatter = treachery MicrometeoroidDarkMatter Cards.micrometeoroidDarkMatter

instance RunMessage MicrometeoroidDarkMatter where
  runMessage msg t@(MicrometeoroidDarkMatter attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ withI18n do
        countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
        campaignI18n
          $ labeled' "micrometeoroid.discardEachEvent"
          $ push
          $ toMessage
          $ discardAll iid attrs (CardWithType EventType)
      pure t
    _ -> MicrometeoroidDarkMatter <$> liftRunMessage msg attrs
