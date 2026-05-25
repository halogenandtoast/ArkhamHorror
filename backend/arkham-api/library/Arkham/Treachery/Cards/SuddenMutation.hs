module Arkham.Treachery.Cards.SuddenMutation (suddenMutation) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Mutated))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SuddenMutation = SuddenMutation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suddenMutation :: TreacheryCard SuddenMutation
suddenMutation = treachery SuddenMutation Cards.suddenMutation

instance HasAbilities SuddenMutation where
  getAbilities (SuddenMutation attrs) = case attrs.attached of
    Just (EnemyTarget eid) ->
      [mkAbility attrs 1 $ forced $ EnemyDefeated #after Anyone ByAny $ EnemyWithId eid]
    _ -> []

instance RunMessage SuddenMutation where
  runMessage msg t@(SuddenMutation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyTo iid NonEliteEnemy
      if null enemies
        then findAndDrawEncounterCard iid (#enemy <> CardWithTrait Mutated)
        else chooseOrRunOneM iid $ targets enemies $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached \case
        EnemyTarget eid -> withLocationOf eid \lid ->
          discardUntilFirst iid (proxy lid attrs) Deck.EncounterDeck #enemy
        _ -> pure ()
      pure t
    RequestedEncounterCard (ProxySource (LocationSource lid) (isSource attrs -> True)) _ (Just card) -> do
      createEnemyAt_ card lid
      pure t
    _ -> SuddenMutation <$> liftRunMessage msg attrs
