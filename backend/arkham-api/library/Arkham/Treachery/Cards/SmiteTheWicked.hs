module Arkham.Treachery.Cards.SmiteTheWicked (smiteTheWicked) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Enemy.Creation
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SmiteTheWicked = SmiteTheWicked TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smiteTheWicked :: TreacheryCard SmiteTheWicked
smiteTheWicked = treachery SmiteTheWicked Cards.smiteTheWicked

instance HasAbilities SmiteTheWicked where
  getAbilities (SmiteTheWicked a) =
    [restricted a 1 criteria $ forcedOnElimination iid | iid <- maybeToList a.owner]
   where
    criteria = maybe Never (exists . EnemyWithId) a.placement.attachedTo.enemy

instance RunMessage SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      whenM (can.target.encounterDeck iid) do
        key <- getEncounterDeckKey iid
        discardUntilFirst iid attrs key #enemy
      pure t
    RequestedEncounterCard (isSource attrs -> True) _ (Just card) -> do
      for_ attrs.owner \ownerId -> do
        attachTreachery attrs
          =<< createEnemyWith card (FarthestLocationFromInvestigator (be ownerId) Anywhere) \x -> x {enemyCreationInvestigator = Just ownerId}
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.owner (`sufferMentalTrauma` 1)
      pure t
    _ -> SmiteTheWicked <$> liftRunMessage msg attrs
