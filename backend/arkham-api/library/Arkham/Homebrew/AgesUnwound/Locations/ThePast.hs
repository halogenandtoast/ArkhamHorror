module Arkham.Homebrew.AgesUnwound.Locations.ThePast (thePast) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Message (pattern R3)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorDiscard, InvestigatorHand))
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Projection

newtype ThePast = ThePast LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Set aside at setup; act 2b puts it into play with The Present and The Future.
It has no unrevealed side of its own (its back is flavour only), so it enters
revealed.
-}
thePast :: LocationCard ThePast
thePast =
  symbolLabel $ locationWith ThePast Cards.thePast 4 (PerPlayer 1) $ revealedL .~ True

{- | "__Forced__ - At the start of your turn: Shuffle a random card from your hand
into your deck. Add a random non-weakness card from your discard pile to your
hand. /
[action][action]: You bring together the threads of the past and present,
resetting your encounter with Aforgomon to its beginning. __→R3__. (Max once per
campaign.)"

The campaign limit is the engine's 'PerCampaign' ability limit rather than a
campaign-log read: Resolution 3 is what sends the table back here, and a replay
starts a new game in which a log-based gate would have to be hand-maintained.
-}
instance HasAbilities ThePast where
  getAbilities (ThePast a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ TurnBegins #when You
      , groupLimit PerCampaign $ restricted a 2 Here doubleActionAbility
      ]

instance RunMessage ThePast where
  runMessage msg l@(ThePast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hand <- fieldMap InvestigatorHand (filter (`cardMatch` NonWeakness)) iid
      for_ (nonEmpty hand) \cards -> do
        card <- sample cards
        push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
      discarded <- fieldMap InvestigatorDiscard (filter ((`cardMatch` NonWeakness) . PlayerCard)) iid
      for_ (nonEmpty discarded) \cards -> do
        card <- sample cards
        addToHand iid [PlayerCard card]
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R3
      pure l
    _ -> ThePast <$> liftRunMessage msg attrs
