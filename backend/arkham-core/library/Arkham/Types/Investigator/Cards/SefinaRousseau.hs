module Arkham.Types.Investigator.Cards.SefinaRousseau where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait

newtype SefinaRousseau = SefinaRousseau InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env SefinaRousseau where
  getModifiersFor source target (SefinaRousseau attrs) =
    getModifiersFor source target attrs

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
  getTokenValue (SefinaRousseau attrs) iid token =
    getTokenValue attrs iid token

instance InvestigatorRunner env => HasActions env SefinaRousseau where
  getActions i window (SefinaRousseau attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env SefinaRousseau where
  runMessage msg i@(SefinaRousseau attrs) = case msg of
    ResolveToken _ ElderSign iid | iid == toId attrs -> pure i
    DrawStartingHand iid | iid == toId attrs -> do
      let
        (discard', hand, deck) = drawOpeningHand attrs 13
        events = filter ((== EventType) . toCardType) hand
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
    _ -> SefinaRousseau <$> runMessage msg attrs
