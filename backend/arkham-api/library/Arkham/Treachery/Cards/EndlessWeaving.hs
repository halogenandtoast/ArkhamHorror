module Arkham.Treachery.Cards.EndlessWeaving (endlessWeaving) where

import Arkham.Attack
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Spider))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EndlessWeaving = EndlessWeaving TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessWeaving :: TreacheryCard EndlessWeaving
endlessWeaving = treachery EndlessWeaving Cards.endlessWeaving

instance RunMessage EndlessWeaving where
  runMessage msg t@(EndlessWeaving attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ EnemyWithTrait Spider
      case enemies of
        [] -> findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Spider
        xs -> do
          chooseOneM iid $ targets xs $ handleTarget iid attrs
          shuffleDeck Deck.EncounterDeck
      pure t
    HandleTargetChoice _ (isSource attrs -> True) (EnemyTarget eid) -> do
      placement <- field EnemyPlacement eid
      let
        eid' =
          case placement of
            AsSwarm hostId _ -> hostId
            _ -> eid
      selectOne (investigatorEngagedWith eid') >>= \case
        Just iid -> do
          swarms <- select $ SwarmOf eid'
          for_ (eid' : swarms) \enemy -> push $ EnemyWillAttack (enemyAttack enemy attrs iid)
        Nothing -> withLocationOf eid' $ placeDoomOn attrs 1

      pure t
    _ -> EndlessWeaving <$> liftRunMessage msg attrs
