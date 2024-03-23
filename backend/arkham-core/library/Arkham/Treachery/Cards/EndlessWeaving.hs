module Arkham.Treachery.Cards.EndlessWeaving (
  endlessWeaving,
  EndlessWeaving (..),
)
where

import Arkham.Attack
import Arkham.Card.CardType
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
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
        [] -> findAndDrawEncounterCard iid $ CardWithType EnemyType <> CardWithTrait Spider
        xs -> do
          chooseOne
            iid
            [targetLabel eid [HandleTargetChoice iid (toSource attrs) (toTarget eid)] | eid <- xs]
          push $ ShuffleDeck Deck.EncounterDeck
      pure t
    HandleTargetChoice _ (isSource attrs -> True) (EnemyTarget eid) -> do
      placement <- field EnemyPlacement eid
      let
        eid' =
          case placement of
            AsSwarm hostId _ -> hostId
            _ -> eid
      miid <- selectOne $ investigatorEngagedWith eid'
      for_ miid \iid -> do
        swarms <- select $ SwarmOf eid'
        for_ (eid' : swarms) \enemy -> push $ EnemyWillAttack (enemyAttack enemy attrs iid)

      pure t
    _ -> EndlessWeaving <$> lift (runMessage msg attrs)
