module Arkham.Investigator.Cards.SefinaRousseau where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner

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
    [ doesNotProvokeAttacksOfOpportunity $ restrictedAbility attrs 1 Self actionAbility
    | notNull attrs.cardsUnderneath
    ]

instance RunMessage SefinaRousseau where
  runMessage msg i@(SefinaRousseau attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [targetLabel (toCardId card) [addToHand (toId i) card] | card <- attrs.cardsUnderneath]
      pure i
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      player <- getPlayer iid
      pushWhen (notNull $ investigatorCardsUnderneath attrs)
        $ chooseOne player
        $ Done "Do not use elder sign ability"
        : [targetLabel (toCardId card) [addToHand (toId i) card] | card <- attrs.cardsUnderneath]
      pure i
    DrawStartingHand iid | iid == toId attrs -> do
      player <- getPlayer iid
      (discard', hand, deck) <- drawOpeningHand attrs 13
      let
        events =
          filter (and . sequence [(== EventType) . toCardType, (/= Events.thePaintedWorld) . toCardDef]) hand
      pushAll [ShuffleDiscardBackIn iid, CheckHandSize $ toId attrs]
      pushWhen (notNull events)
        $ chooseUpToN player 5 "Done Choosing Events"
        $ [ targetLabel (toCardId event)
            $ [RemoveCardFromHand iid (toCardId event), PlaceUnderneath (toTarget iid) [event]]
          | event <- events
          ]
      pure . SefinaRousseau $ attrs & discardL .~ discard' & handL .~ hand & deckL .~ Deck deck
    InvestigatorMulligan iid | iid == toId attrs -> pure i
    _ -> SefinaRousseau <$> runMessage msg attrs
