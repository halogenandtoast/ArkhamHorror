module Arkham.Treachery.Cards.BaneOfTheLiving (
  baneOfTheLiving,
  BaneOfTheLiving (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher hiding (StoryCard)
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Story.Types (Field (..))
import Arkham.Trait (Trait (Geist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BaneOfTheLiving = BaneOfTheLiving TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baneOfTheLiving :: TreacheryCard BaneOfTheLiving
baneOfTheLiving = treachery BaneOfTheLiving Cards.baneOfTheLiving

instance RunMessage BaneOfTheLiving where
  runMessage msg t@(BaneOfTheLiving attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasUnfinishedBusiness <- selectAny $ StoryWithTitle "Unfinished Business"
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Choose an Unfinished Business card in play, flip it to its Heretic side, and place damage on it equal to half its health."
            [RevelationChoice iid (toSource attrs) 1]
          | hasUnfinishedBusiness
          ]
        <> [ Label
              "Discard cards from the top of the spectral encounter deck until a Geist enemy is discarded. Spawn that enemy engaged with you."
              [RevelationChoice iid (toSource attrs) 2]
           ]
      pure t
    RevelationChoice iid (isSource attrs -> True) 1 -> do
      stories <- selectWithField StoryCard $ StoryWithTitle "Unfinished Business"
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel
            story'
            [ Flip iid (toSource attrs) (toTarget story')
            , HandleTargetChoice iid (toSource attrs) (CardIdTarget $ toCardId card)
            ]
          | (story', card) <- stories
          ]
      pure t
    HandleTargetChoice _ (isSource attrs -> True) (CardIdTarget cardId) -> do
      e <- selectJust $ EnemyWithCardId cardId
      health <- fieldJust EnemyHealth e
      push $ PlaceDamage (toSource attrs) (toTarget e) (health `div` 2)
      pure t
    RevelationChoice iid (isSource attrs -> True) 2 -> do
      push
        $ DiscardUntilFirst
          iid
          (toSource attrs)
          (Deck.EncounterDeckByKey SpectralEncounterDeck)
          (BasicCardMatch $ CardWithType EnemyType <> CardWithTrait Geist)
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) -> do
      creation <- createEnemy card iid
      push $ toMessage creation
      pure t
    _ -> BaneOfTheLiving <$> runMessage msg attrs
