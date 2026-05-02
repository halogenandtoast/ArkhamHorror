module Arkham.Treachery.Cards.Bloom (bloom) where

import Arkham.Deck qualified as Deck
import Arkham.Helpers.GameValue
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Flora))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Bloom = Bloom TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloom :: TreacheryCard Bloom
bloom = treachery Bloom Cards.bloom

instance RunMessage Bloom where
  runMessage msg t@(Bloom attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyToFallback iid (EnemyWithTrait Flora <> EnemyWithAnyDamage)
      if null enemies
        then discardUntilFirst iid attrs Deck.EncounterDeck $ basic $ #enemy <> CardWithTrait Flora
        else chooseTargetM iid enemies (handleTarget iid attrs)
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      for_ mcard (drawCard iid)
      pure t
    HandleTargetChoice _iid (isSource attrs -> True) (EnemyTarget eid) -> do
      isElite <- matches eid EliteEnemy
      if isElite
        then do
          n <- perPlayer 2
          healDamage eid attrs n
        else healAllDamage attrs eid
      pure t
    _ -> Bloom <$> liftRunMessage msg attrs
