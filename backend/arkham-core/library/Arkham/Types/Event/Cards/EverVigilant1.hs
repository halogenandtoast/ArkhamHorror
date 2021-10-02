module Arkham.Types.Event.Cards.EverVigilant1
  ( everVigilant1
  , EverVigilant1(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher hiding (DuringTurn)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window

newtype EverVigilant1 = EverVigilant1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everVigilant1 :: EventCard EverVigilant1
everVigilant1 = event EverVigilant1 Cards.everVigilant1

instance HasModifiersFor env EverVigilant1 where
  getModifiersFor _ (InvestigatorTarget iid) (EverVigilant1 attrs)
    | iid == eventOwner attrs = pure
    $ toModifiers attrs [ReduceCostOf AnyCard 1]
  getModifiersFor _ _ _ = pure []

instance CanCheckPlayable env => RunMessage env EverVigilant1 where
  runMessage msg e@(EverVigilant1 attrs) = case msg of
    InvestigatorPlayEvent iid eid mtarget _ _ | eid == toId attrs -> do
      e <$ pushAll
        (replicate 3 (ResolveEvent iid eid mtarget)
        <> [Discard (toTarget attrs)]
        )
    ResolveEvent iid eid mtarget | eid == toId attrs -> do
      cards <- getList @Card
        (InHandOf (InvestigatorWithId iid)
        <> BasicCardMatch (CardWithType AssetType)
        )
      playableCards <- filterM
        (getIsPlayable
          iid
          (toSource attrs)
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
