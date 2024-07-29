module Arkham.Treachery.Cards.WheresPa (wheresPa, WheresPa (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Creation
import Arkham.Helpers.Message (createEnemy)
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.Scenario (getEncounterDeckKey)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WheresPa = WheresPa TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wheresPa :: TreacheryCard WheresPa
wheresPa = treachery WheresPa Cards.wheresPa

instance HasModifiersFor WheresPa where
  getModifiersFor target (WheresPa a) | target `elem` a.attached = do
    modified a [AddKeyword Keyword.Elusive]
  getModifiersFor _ _ = pure []

instance HasAbilities WheresPa where
  getAbilities (WheresPa a) =
    [ restrictedAbility a 1 (youExist $ investigatorIs Investigators.hankSamson)
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage WheresPa where
  runMessage msg t@(WheresPa attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasEncounterDeck <- can.target.encounterDeck iid
      when hasEncounterDeck $ do
        key <- getEncounterDeckKey iid
        push $ DiscardUntilFirst iid (toSource attrs) (Deck.EncounterDeckByKey key) #enemy
      pure t
    RequestedEncounterCard (isSource attrs -> True) _ mcard -> do
      for_ mcard $ \card -> do
        let ownerId = fromJustNote "has to be set" attrs.owner
        connectedLocations <- selectAny $ ConnectedFrom (locationWithInvestigator ownerId)
        enemyCreation <-
          createEnemy
            card
            $ if connectedLocations
              then ConnectedFrom (locationWithInvestigator ownerId)
              else locationWithInvestigator ownerId
        push $ toMessage $ enemyCreation {enemyCreationInvestigator = Just ownerId}
        attachTreachery attrs (enemyCreationEnemyId enemyCreation)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directHorror iid (attrs.ability 1) 1
      pure t
    _ -> WheresPa <$> liftRunMessage msg attrs
