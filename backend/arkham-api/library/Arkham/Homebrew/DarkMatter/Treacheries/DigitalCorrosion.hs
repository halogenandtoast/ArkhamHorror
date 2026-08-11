module Arkham.Homebrew.DarkMatter.Treacheries.DigitalCorrosion (digitalCorrosion) where

import Arkham.Discard
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Message.Discard (discardFromHand)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype DigitalCorrosion = DigitalCorrosion TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digitalCorrosion :: TreacheryCard DigitalCorrosion
digitalCorrosion = treachery DigitalCorrosion Cards.digitalCorrosion

{- | "Revelation - Test [willpower] (X). X is the shroud value of your location.
For each point you fail by, discard a card from your play area and/or from your
hand."
-}
instance RunMessage DigitalCorrosion where
  runMessage msg t@(DigitalCorrosion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      lid <- getJustLocation iid
      shroud <- fromMaybe 0 <$> field LocationShroud lid
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed shroud)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ toMessage $ discardFromHand iid attrs DiscardChoose n
      pure t
    _ -> DigitalCorrosion <$> liftRunMessage msg attrs
