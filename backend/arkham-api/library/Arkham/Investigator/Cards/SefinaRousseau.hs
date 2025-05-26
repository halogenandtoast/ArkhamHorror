module Arkham.Investigator.Cards.SefinaRousseau where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers
import Arkham.Helpers.Investigator (drawOpeningHand)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (discardL, handL)
import Arkham.Matcher.Card
import Arkham.Message.Lifted.Choose

newtype SefinaRousseau = SefinaRousseau InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

sefinaRousseau :: InvestigatorCard SefinaRousseau
sefinaRousseau =
  investigator SefinaRousseau Cards.sefinaRousseau
    $ Stats {health = 5, sanity = 9, willpower = 4, intellect = 2, combat = 2, agility = 4}

instance HasChaosTokenValue SefinaRousseau where
  getChaosTokenValue iid ElderSign (SefinaRousseau attrs) | iid == investigatorId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 3)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities SefinaRousseau where
  getAbilities (SefinaRousseau attrs) =
    [noAOO $ restricted attrs 1 Self actionAbility | notNull attrs.cardsUnderneath]

instance RunMessage SefinaRousseau where
  runMessage msg i@(SefinaRousseau attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseTargetM iid attrs.cardsUnderneath (addToHand iid . only)
      pure i
    ElderSignEffect (is attrs -> True) -> do
      chooseOrRunOneM attrs.id do
        labeled "Do not use elder sign ability" nothing
        targets attrs.cardsUnderneath (addToHand attrs.id . only)
      pure i
    DrawStartingHand (is attrs -> True) -> do
      (discard', hand, deck) <- drawOpeningHand attrs 13
      let events = filterCards (#event <> not_ (cardIs Events.thePaintedWorld)) hand
      when (notNull events) do
        chooseUpToNM attrs.id 5 "Done Choosing Events" $ targets events (placeUnderneath attrs.id . only)
      push $ CheckHandSize attrs.id
      shuffleDiscardBackIn attrs.id
      pure . SefinaRousseau $ attrs & discardL .~ discard' & handL .~ hand & deckL .~ Deck deck
    InvestigatorMulligan (is attrs -> True) -> pure i
    _ -> SefinaRousseau <$> liftRunMessage msg attrs
