module Arkham.Event.Cards.EverVigilant1
  ( everVigilant1
  , EverVigilant1(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Attrs (Field(..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message
import Arkham.Modifier
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype EverVigilant1 = EverVigilant1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everVigilant1 :: EventCard EverVigilant1
everVigilant1 = event EverVigilant1 Cards.everVigilant1

instance HasModifiersFor EverVigilant1 where
  getModifiersFor _ (InvestigatorTarget iid) (EverVigilant1 attrs)
    | iid == eventOwner attrs = pure
    $ toModifiers attrs [ReduceCostOf AnyCard 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage EverVigilant1 where
  runMessage msg e@(EverVigilant1 attrs) = case msg of
    InvestigatorPlayEvent iid eid mtarget _ _ | eid == toId attrs -> do
      e <$ pushAll
        (replicate 3 (ResolveEvent iid eid mtarget)
        <> [Discard (toTarget attrs)]
        )
    ResolveEvent iid eid mtarget | eid == toId attrs -> do
      cards <- fieldMap InvestigatorHand (filter (`cardMatch` CardWithType AssetType)) iid
      playableCards <- filterM
        (getIsPlayable
          iid
          (toSource attrs)
          UnpaidCost
          [Window Timing.When (DuringTurn iid), Window Timing.When NonFast]
        )
        cards
      e <$ when
        (notNull playableCards)
        (push
          (chooseUpToN
            iid
            1
            "Do not play asset"
            [ Run
                [ PayCardCost iid (toCardId c)
                , InitiatePlayCard iid (toCardId c) mtarget False
                ]
            | c <- playableCards
            ]
          )
        )
    _ -> EverVigilant1 <$> runMessage msg attrs
