module Arkham.Treachery.Cards.Tekelili_224 (tekelili_224) where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Tekelili_224 = Tekelili_224 TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tekelili_224 :: TreacheryCard Tekelili_224
tekelili_224 = treachery Tekelili_224 Cards.tekelili_224

instance RunMessage Tekelili_224 where
  runMessage msg t@(Tekelili_224 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- ifM_ (getShouldResolveEffectsAgain iid attrs) 2 1
      repeated n $ assignDamage iid attrs 1
      resolveTekelili iid attrs
      pure t
    _ -> Tekelili_224 <$> liftRunMessage msg attrs
