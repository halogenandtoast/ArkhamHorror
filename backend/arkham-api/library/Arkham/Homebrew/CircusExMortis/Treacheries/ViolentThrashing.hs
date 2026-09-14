module Arkham.Homebrew.CircusExMortis.Treacheries.ViolentThrashing (violentThrashing) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Treachery.Import.Lifted

newtype ViolentThrashing = ViolentThrashing TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

violentThrashing :: TreacheryCard ViolentThrashing
violentThrashing = treachery ViolentThrashing Cards.violentThrashing

instance RunMessage ViolentThrashing where
  runMessage msg t@(ViolentThrashing attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      revealFuryToken attrs
      pure t
    _ -> ViolentThrashing <$> liftRunMessage msg attrs
