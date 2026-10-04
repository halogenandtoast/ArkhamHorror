module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.Wrecked (wrecked) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.Malfunction qualified as Malfunction
import Arkham.Treachery.Import.Lifted

newtype Wrecked = Wrecked TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrecked :: TreacheryCard Wrecked
wrecked = treachery Wrecked Cards.wrecked

{- | "Flip each 'Running' vehicle story asset with an attached Malfunction card to its
'Stopped' side. If no card has been flipped this way, Wrecked! gains surge."
-}
instance RunMessage Wrecked where
  runMessage msg t@(Wrecked attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      running <-
        select
          $ mapOneOf
            assetIs
            [Assets.thomasDawsonsCarRunning, Assets.elinaHarpersCarRunning]
          <> AssetWithAttachedTreachery (treacheryIs Malfunction.malfunction)
      if null running
        then gainSurge attrs
        else for_ running (push . Flip iid (toSource attrs) . toTarget)
      pure t
    _ -> Wrecked <$> liftRunMessage msg attrs
