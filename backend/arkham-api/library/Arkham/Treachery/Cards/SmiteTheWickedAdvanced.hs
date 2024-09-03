module Arkham.Treachery.Cards.SmiteTheWickedAdvanced where

import Arkham.Ability
import Arkham.Capability
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Creation
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SmiteTheWickedAdvanced = SmiteTheWickedAdvanced TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smiteTheWickedAdvanced :: TreacheryCard SmiteTheWickedAdvanced
smiteTheWickedAdvanced = treachery SmiteTheWickedAdvanced Cards.smiteTheWickedAdvanced

instance HasModifiersFor SmiteTheWickedAdvanced where
  getModifiersFor (EnemyTarget eid) (SmiteTheWickedAdvanced attrs) | treacheryOnEnemy eid attrs = do
    modified attrs [EnemyFight 2, HealthModifier 2]
  getModifiersFor _ _ = pure []

instance HasAbilities SmiteTheWickedAdvanced where
  getAbilities (SmiteTheWickedAdvanced a) =
    [mkAbility a 1 $ forcedOnElimination iid | iid <- maybeToList (treacheryOwner a)]

instance RunMessage SmiteTheWickedAdvanced where
  runMessage msg t@(SmiteTheWickedAdvanced attrs) = case msg of
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
    _ -> SmiteTheWickedAdvanced <$> runMessage msg attrs
