module Arkham.Event.Cards.SmuggledGoods
  ( smuggledGoods
  , SmuggledGoods(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Trait
import Arkham.Zone

newtype SmuggledGoods = SmuggledGoods EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smuggledGoods :: EventCard SmuggledGoods
smuggledGoods = event SmuggledGoods Cards.smuggledGoods

instance RunMessage SmuggledGoods where
  runMessage msg e@(SmuggledGoods attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      hasIllicitCardInDiscard <- fieldP
        InvestigatorDiscard
        (any (member Illicit . toTraits))
        iid
      pushAll
        [ chooseOrRunOne
          iid
          (Label
              "Search deck"
              [ Search
                  iid
                  (toSource attrs)
                  (InvestigatorTarget iid)
                  [fromTopOfDeck 9]
                  (CardWithTrait Illicit)
                  (DrawFound iid 1)
              ]
          : [ Label
                "Search discard"
                [ Search
                    iid
                    (toSource attrs)
                    (toTarget attrs)
                    [(FromDiscard, PutBack)]
                    (CardWithTrait Illicit)
                    (DrawFound iid 1)
                ]
            | hasIllicitCardInDiscard
            ]
          )
        ]
      pure e
    _ -> SmuggledGoods <$> runMessage msg attrs
