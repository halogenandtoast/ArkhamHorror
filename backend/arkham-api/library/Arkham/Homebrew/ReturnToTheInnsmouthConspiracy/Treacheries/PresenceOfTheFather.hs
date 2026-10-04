module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.PresenceOfTheFather (
  presenceOfTheFather,
) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype PresenceOfTheFather = PresenceOfTheFather TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

presenceOfTheFather :: TreacheryCard PresenceOfTheFather
presenceOfTheFather = treachery PresenceOfTheFather Cards.presenceOfTheFather

{- | "Whenever an investigator within 1 location of Lair of Dagon resolves the Forced
ability on a Dagon's Brood enemy, they have to choose all possible options instead of
choosing only one." The Brood cards read 'MustResolveAllOptions' and switch from
'chooseOneM' to 'chooseOneAtATimeM', which is what resolving every option means.
-}
instance HasModifiersFor PresenceOfTheFather where
  getModifiersFor (PresenceOfTheFather a) = case a.placement of
    AttachedToLocation lid ->
      modifySelect
        a
        (InvestigatorAt $ LocationWithDistanceFromAtMost 1 (LocationWithId lid) Anywhere)
        [MustResolveAllOptions]
    _ -> pure mempty

instance RunMessage PresenceOfTheFather where
  runMessage msg t@(PresenceOfTheFather attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      selectOne (locationIs Locations.lairOfDagon)
        >>= traverse_ (placeTreachery attrs . AttachedToLocation)
      pure t
    _ -> PresenceOfTheFather <$> liftRunMessage msg attrs
