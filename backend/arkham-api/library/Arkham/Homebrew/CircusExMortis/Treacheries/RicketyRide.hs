module Arkham.Homebrew.CircusExMortis.Treacheries.RicketyRide (ricketyRide) where

import Arkham.Distance (unDistance)
import {-# SOURCE #-} Arkham.GameEnv (getDistance)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype RicketyRide = RicketyRide TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ricketyRide :: TreacheryCard RicketyRide
ricketyRide = treachery RicketyRide Cards.ricketyRide

instance HasModifiersFor RicketyRide where
  getModifiersFor (RicketyRide attrs) = do
    whenJustM getSkillTest \st -> maybeModified_ attrs (SkillTestTarget st.id) do
      guard $ isSource attrs st.source
      loc <- MaybeT $ getLocationOf st.investigator
      engine <- MaybeT $ selectOne (locationIs Locations.locomotiveEngine)
      dist <- MaybeT $ getDistance loc engine
      pure [Difficulty (unDistance dist)]

instance HasAbilities RicketyRide

instance RunMessage RicketyRide where
  runMessage msg t@(RicketyRide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOne (assetIs Assets.carrieDykstra) >>= traverse_ (exhaustEnemy attrs)
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #agility] \sType ->
          skillLabeled sType $ beginSkillTest sid iid attrs iid sType (Fixed 1)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      loseResources iid attrs 2
      pure t
    _ -> RicketyRide <$> liftRunMessage msg attrs
