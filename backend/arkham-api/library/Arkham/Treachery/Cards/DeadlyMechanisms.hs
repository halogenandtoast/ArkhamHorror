module Arkham.Treachery.Cards.DeadlyMechanisms (deadlyMechanisms) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.ForMovement
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Scenarios.TheGrandVault.Helpers
import Arkham.Trait (Trait (Vault))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeadlyMechanisms = DeadlyMechanisms TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlyMechanisms :: TreacheryCard DeadlyMechanisms
deadlyMechanisms = treachery DeadlyMechanisms Cards.deadlyMechanisms

instance RunMessage DeadlyMechanisms where
  runMessage msg t@(DeadlyMechanisms attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      -- Choose two instead of one if it is act 2.
      n <- getCurrentActStep
      let numChoices = if n >= 2 then 2 else 1

      -- Deactivate the nearest activated Vault location.
      nearestVault <- select $ NearestLocationTo iid (activatedLocation <> LocationWithTrait Vault)
      -- Draw 1 of the set-aside Vault Attendant enemies.
      attendants <- getSetAsideCardsMatching (cardIs Enemies.vaultAttendant)
      -- Move to an adjacent location (flooded if able).
      flooded <- select $ ConnectedFrom ForMovement (locationWithInvestigator iid) <> FloodedLocation
      allAdjacent <- select $ ConnectedFrom ForMovement (locationWithInvestigator iid)
      let adjacent = if null flooded then allAdjacent else flooded

      chooseNM iid numChoices $ scenarioI18n do
        unless (null nearestVault) do
          labeled' "deadlyMechanisms.deactivate" do
            chooseTargetM iid nearestVault $ deactivateLocation attrs
        unless (null attendants) do
          labeled' "deadlyMechanisms.drawAttendant" $ for_ (take 1 attendants) (drawCard iid)
        unless (null adjacent) do
          labeled' "deadlyMechanisms.move" do
            chooseTargetM iid adjacent $ moveTo attrs iid
      pure t
    _ -> DeadlyMechanisms <$> liftRunMessage msg attrs
