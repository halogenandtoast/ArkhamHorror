module Arkham.Treachery.Cards.SmiteTheWicked where

import Arkham.Ability
import Arkham.Capability
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Creation
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SmiteTheWicked = SmiteTheWicked TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

smiteTheWicked :: TreacheryCard SmiteTheWicked
smiteTheWicked = treachery SmiteTheWicked Cards.smiteTheWicked

instance HasAbilities SmiteTheWicked where
  getAbilities (SmiteTheWicked a) =
    [mkAbility a 1 $ forcedOnElimination iid | iid <- maybeToList (treacheryOwner a)]

instance RunMessage SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasEncounterDeck <- can.target.encounterDeck iid
      when hasEncounterDeck $ do
        key <- getEncounterDeckKey iid
        push $ DiscardUntilFirst iid (toSource attrs) (Deck.EncounterDeckByKey key) #enemy
      pure t
    RequestedEncounterCard (isSource attrs -> True) _ mcard -> do
      for_ mcard $ \card -> do
        let ownerId = fromJustNote "has to be set" attrs.owner
        enemyCreation <-
          createEnemy card $ FarthestLocationFromInvestigator (InvestigatorWithId ownerId) Anywhere
        pushAll
          [ toMessage $ enemyCreation {enemyCreationInvestigator = Just ownerId}
          , attachTreachery attrs (enemyCreationEnemyId enemyCreation)
          ]
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let investigator = fromJustNote "missing investigator" attrs.owner
      push $ SufferTrauma investigator 0 1
      pure t
    _ -> SmiteTheWicked <$> runMessage msg attrs
