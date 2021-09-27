module Arkham.Types.Investigator.Cards.SefinaRousseau where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Events
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Target

newtype SefinaRousseau = SefinaRousseau InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

sefinaRousseau :: SefinaRousseau
sefinaRousseau = SefinaRousseau $ baseAttrs
  "03003"
  "Sefina Rousseau"
  Rogue
  Stats
    { health = 5
    , sanity = 9
    , willpower = 4
    , intellect = 2
    , combat = 2
    , agility = 4
    }
  [Artist]

instance HasTokenValue env SefinaRousseau where
  getTokenValue (SefinaRousseau attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 3)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance HasAbilities SefinaRousseau where
  getAbilities (SefinaRousseau attrs) =
    [ restrictedAbility attrs 1 Self (ActionAbility Nothing $ ActionCost 1)
        & (abilityDoesNotProvokeAttacksOfOpportunityL .~ True)
    | notNull (investigatorCardsUnderneath attrs)
    ]

instance InvestigatorRunner env => RunMessage env SefinaRousseau where
  runMessage msg i@(SefinaRousseau attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> i <$ push
      (chooseOne
        (toId i)
        [ TargetLabel (CardIdTarget $ toCardId card) [AddToHand (toId i) card]
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
                (CardIdTarget $ toCardId card)
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
