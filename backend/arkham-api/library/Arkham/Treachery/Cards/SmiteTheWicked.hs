module Arkham.Treachery.Cards.SmiteTheWicked (smiteTheWicked) where

import Arkham.Deck qualified as Deck
import Arkham.Ability
import Arkham.Enemy.Creation
import Arkham.Matcher
import Arkham.Placement
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
      discardUntilFirst iid attrs Deck.EncounterDeck #enemy
      pure t
    RequestedEncounterCard (isSource attrs -> True) _ (Just card) -> do
      focusCards [card] do
        for_ attrs.owner \ownerId -> do
          createEnemyWith card (FarthestLocationFromInvestigator (be ownerId) Anywhere) \x ->
            x
              { enemyCreationInvestigator = Just ownerId
              , enemyCreationBefore = [PlaceTreachery (toId attrs) (AttachedToEnemy $ enemyCreationEnemyId x)]
              }
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.owner (`sufferMentalTrauma` 1)
      pure t
    _ -> SmiteTheWicked <$> liftRunMessage msg attrs
