module Arkham.Investigator.Cards.SefinaRousseau where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Message

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

instance HasChaosTokenValue SefinaRousseau where
  getChaosTokenValue iid ElderSign (SefinaRousseau attrs)
    | iid == investigatorId attrs = pure
    $ ChaosTokenValue ElderSign (PositiveModifier 3)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities SefinaRousseau where
  getAbilities (SefinaRousseau attrs) =
    [ restrictedAbility attrs 1 Self (ActionAbility Nothing $ ActionCost 1)
        & (abilityDoesNotProvokeAttacksOfOpportunityL .~ True)
    | notNull (investigatorCardsUnderneath attrs)
    ]

instance RunMessage SefinaRousseau where
  runMessage msg i@(SefinaRousseau attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> i <$ push
      (chooseOne
        (toId i)
        [ TargetLabel (CardIdTarget $ toCardId card) [addToHand (toId i) card]
        | card <- investigatorCardsUnderneath attrs
        ]
      )
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> i <$ when
      (notNull $ investigatorCardsUnderneath attrs)
      (push
        (chooseOne
          (toId i)
          (Done "Do not use elder sign ability"
          : [ TargetLabel
                (CardIdTarget $ toCardId card)
                [addToHand (toId i) card]
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
      pushAll [ShuffleDiscardBackIn iid, CheckHandSize $ toId attrs]
      when
        (notNull events)
        (push
          (chooseUpToN
            iid
            5
            "Done Choosing Events"
            [ TargetLabel
                (CardIdTarget $ toCardId event)
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
