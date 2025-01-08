module Arkham.Treachery.Cards.Tekelili_223 (tekelili_223, Tekelili_223 (..)) where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), hasModifier)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Tekelili_223 = Tekelili_223 TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tekelili_223 :: TreacheryCard Tekelili_223
tekelili_223 = treachery Tekelili_223 Cards.tekelili_223

instance RunMessage Tekelili_223 where
  runMessage msg t@(Tekelili_223 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- ifM_ (hasModifier (toCard attrs) ResolveEffectsAgain) 2 1
      repeated n $ assignHorror iid attrs 1
      resolveTekelili iid attrs
      pure t
    _ -> Tekelili_223 <$> liftRunMessage msg attrs
