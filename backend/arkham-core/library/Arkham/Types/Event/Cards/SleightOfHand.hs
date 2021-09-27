module Arkham.Types.Event.Cards.SleightOfHand
  ( sleightOfHand
  , SleightOfHand(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype SleightOfHand = SleightOfHand EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHand :: EventCard SleightOfHand
sleightOfHand = event SleightOfHand Cards.sleightOfHand

instance Query ExtendedCardMatcher env => RunMessage env SleightOfHand where
  runMessage msg e@(SleightOfHand attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      cards <- selectList
        (InHandOf (InvestigatorWithId iid)
        <> BasicCardMatch (CardWithTrait Item)
        )
      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel
              (CardIdTarget $ toCardId card)
              [ PutCardIntoPlay iid card (Just $ toTarget attrs)
              , CreateEffect
                (toCardCode attrs)
                Nothing
                (toSource attrs)
                (AssetTarget $ AssetId $ toCardId card)
              ]
          | card <- cards
          ]
        , Discard (toTarget attrs)
        ]
    _ -> SleightOfHand <$> runMessage msg attrs
