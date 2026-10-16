module Arkham.Homebrew.TheLegendOfSleepyHollow.Treacheries.WhispersOnTheWind (
  whispersOnTheWind,
) where

import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Treachery.Import.Lifted

newtype WhispersOnTheWind = WhispersOnTheWind TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersOnTheWind :: TreacheryCard WhispersOnTheWind
whispersOnTheWind = treachery WhispersOnTheWind Cards.whispersOnTheWind

{- | "If there is doom on your location, discard 1 additional card regardless of
whether you succeed or fail."
-}
discardForDoom :: ReverseQueue m => InvestigatorId -> TreacheryAttrs -> m ()
discardForDoom iid attrs =
  withLocationOf iid \lid -> whenM (locationIsDoomed lid) $ randomDiscard iid attrs

instance RunMessage WhispersOnTheWind where
  runMessage msg t@(WhispersOnTheWind attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      discardForDoom iid attrs
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      repeated n $ randomDiscard iid attrs
      discardForDoom iid attrs
      pure t
    _ -> WhispersOnTheWind <$> liftRunMessage msg attrs
