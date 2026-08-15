module Arkham.Homebrew.DarkMatter.Treacheries.DigitalCorrosion (digitalCorrosion) where

import Arkham.Calculation
import Arkham.Discard
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message.Discard.Lifted (discardFromHand)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Treachery.Import.Lifted

newtype DigitalCorrosion = DigitalCorrosion TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digitalCorrosion :: TreacheryCard DigitalCorrosion
digitalCorrosion = treachery DigitalCorrosion Cards.digitalCorrosion

instance RunMessage DigitalCorrosion where
  runMessage msg t@(DigitalCorrosion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        sid <- getRandom
        revelationSkillTest sid iid attrs #willpower (LocationMaybeFieldCalculation lid LocationShroud)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      discardFromHand iid attrs DiscardChoose n
      pure t
    _ -> DigitalCorrosion <$> liftRunMessage msg attrs
