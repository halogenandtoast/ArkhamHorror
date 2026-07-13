module Arkham.Homebrew.DarkMatter.Treacheries.Micrometeoroid (micrometeoroid) where

import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Card.CardType
import Arkham.Helpers.Message.Discard (discardAll)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Micrometeoroid = Micrometeoroid TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

micrometeoroid :: TreacheryCard Micrometeoroid
micrometeoroid = treachery Micrometeoroid Cards.micrometeoroid

instance RunMessage Micrometeoroid where
  runMessage msg t@(Micrometeoroid attrs) = runQueueT $ case msg of
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
    _ -> Micrometeoroid <$> liftRunMessage msg attrs
