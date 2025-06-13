module Arkham.Treachery.Cards.BaneOfTheLiving (baneOfTheLiving) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher hiding (StoryCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Story.Types (Field (..))
import Arkham.Trait (Trait (Geist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BaneOfTheLiving = BaneOfTheLiving TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baneOfTheLiving :: TreacheryCard BaneOfTheLiving
baneOfTheLiving = treachery BaneOfTheLiving Cards.baneOfTheLiving

instance RunMessage BaneOfTheLiving where
  runMessage msg t@(BaneOfTheLiving attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasUnfinishedBusiness <- selectAny $ StoryWithTitle "Unfinished Business"
      chooseOrRunOneM iid do
        when hasUnfinishedBusiness do
          labeled
            "Choose an Unfinished Business card in play, flip it to its Heretic side, and place damage on it equal to half its health."
            $ doStep 1 msg
        labeled
          "Discard cards from the top of the spectral encounter deck until a Geist enemy is discarded. Spawn that enemy engaged with you."
          $ doStep 2 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      stories <- selectWithField StoryCard $ StoryWithTitle "Unfinished Business"
      chooseOneM iid $ for_ stories \(story', card) ->
        targeting story' do
          flipOverBy iid attrs story'
          handleTarget iid attrs card.id
      pure t
    HandleTargetChoice _ (isSource attrs -> True) (CardIdTarget cardId) -> do
      e <- selectJust $ EnemyWithCardId cardId
      health <- fieldJust EnemyHealth e
      placeTokens attrs e #damage (health `div` 2)
      pure t
    DoStep 2 (Revelation iid (isSource attrs -> True)) -> do
      discardUntilFirst iid attrs SpectralEncounterDeck (basic $ #enemy <> CardWithTrait Geist)
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) -> do
      createEnemy_ card iid
      pure t
    _ -> BaneOfTheLiving <$> liftRunMessage msg attrs
