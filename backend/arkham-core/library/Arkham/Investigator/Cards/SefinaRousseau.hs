module Arkham.Investigator.Cards.SefinaRousseau where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Helpers
import Arkham.Investigator.Runner
import Arkham.Message
import Arkham.Target

newtype SefinaRousseau = SefinaRousseau InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sefinaRousseau :: InvestigatorCard SefinaRousseau
sefinaRousseau = investigator
  SefinaRousseau
  Cards.sefinaRousseau
  Stats
    { health = 5
    , sanity = 9
    , willpower = 4
    , intellect = 2
    , combat = 2
    , agility = 4
    }

instance HasTokenValue SefinaRousseau where
  getTokenValue iid ElderSign (SefinaRousseau attrs)
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 3)
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance HasAbilities SefinaRousseau where
  getAbilities (SefinaRousseau attrs) =
    [ restrictedAbility attrs 1 Self (ActionAbility Nothing $ ActionCost 1)
        & (abilityDoesNotProvokeAttacksOfOpportunityL .~ True)
    | notNull (investigatorCardsUnderneath attrs)
    ]

instance RunMessage SefinaRousseau where
  runMessage msg i@(SefinaRousseau attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> i <$ push
      (chooseOne
        (toId i)
        [ TargetLabel (CardTarget card) [AddToHand (toId i) card]
        | card <- investigatorCardsUnderneath attrs
        ]
      )
    ResolveToken _ ElderSign iid | iid == toId attrs -> i <$ when
      (notNull $ investigatorCardsUnderneath attrs)
      (push
        (chooseOne
          (toId i)
          (Done "Do not use elder sign ability"
          : [ TargetLabel
                (CardTarget card)
                [AddToHand (toId i) card]
            | card <- investigatorCardsUnderneath attrs
            ]
          )
        )
      )
    DrawStartingHand iid | iid == toId attrs -> do
      let
        (discard', hand, deck) = drawOpeningHand attrs 13
        events = filter
          (and
          . sequence
              [ (== EventType) . toCardType
              , (/= Events.thePaintedWorld) . toCardDef
              ]
          )
          hand
      push (CheckHandSize $ toId attrs)
      when
        (notNull events)
        (push
          (chooseUpToN
            iid
            5
            "Done Choosing Events"
            [ TargetLabel
                (CardTarget event)
                [ RemoveCardFromHand iid (toCardId event)
                , PlaceUnderneath (InvestigatorTarget iid) [event]
                ]
            | event <- events
            ]
          )
        )
      pure
        . SefinaRousseau
        $ attrs
        & (discardL .~ discard')
        & (handL .~ hand)
        & (deckL .~ Deck deck)
    InvestigatorMulligan iid | iid == toId attrs -> pure i
    _ -> SefinaRousseau <$> runMessage msg attrs
