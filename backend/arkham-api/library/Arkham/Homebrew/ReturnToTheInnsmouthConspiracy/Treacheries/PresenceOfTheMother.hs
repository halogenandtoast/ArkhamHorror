module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.PresenceOfTheMother (
  presenceOfTheMother,
) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype PresenceOfTheMother = PresenceOfTheMother TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

presenceOfTheMother :: TreacheryCard PresenceOfTheMother
presenceOfTheMother = treachery PresenceOfTheMother Cards.presenceOfTheMother

{- | "Whenever an investigator within 1 location of Lair of Hydra resolves the Forced
ability on a Hydra's Brood enemy, they have to choose all possible options instead of
choosing only one." The Brood cards read 'MustResolveAllOptions' and switch from
'chooseOneM' to 'chooseOneAtATimeM', which is what resolving every option means.
-}
instance HasModifiersFor PresenceOfTheMother where
  getModifiersFor (PresenceOfTheMother a) = case a.placement of
    AttachedToLocation lid ->
      modifySelect
        a
        (InvestigatorAt $ LocationWithDistanceFromAtMost 1 (LocationWithId lid) Anywhere)
        [MustResolveAllOptions]
    _ -> pure mempty

instance RunMessage PresenceOfTheMother where
  runMessage msg t@(PresenceOfTheMother attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      selectOne (locationIs Locations.lairOfHydra)
        >>= traverse_ (placeTreachery attrs . AttachedToLocation)
      pure t
    _ -> PresenceOfTheMother <$> liftRunMessage msg attrs
