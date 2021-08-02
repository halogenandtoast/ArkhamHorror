module Arkham.Types.Event.Cards.EverVigilant
  ( everVigilant
  , EverVigilant(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Window

newtype EverVigilant = EverVigilant EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everVigilant :: EventCard EverVigilant
everVigilant = event EverVigilant Cards.everVigilant

instance HasActions env EverVigilant where
  getActions iid window (EverVigilant attrs) = getActions iid window attrs

instance HasModifiersFor env EverVigilant where
  getModifiersFor _ (InvestigatorTarget iid) (EverVigilant attrs)
    | iid == eventOwner attrs = pure
    $ toModifiers attrs [ReduceCostOf AnyCard 1]
  getModifiersFor _ _ _ = pure []

instance CanCheckPlayable env => RunMessage env EverVigilant where
  runMessage msg e@(EverVigilant attrs) = case msg of
    InvestigatorPlayEvent iid eid mtarget _ | eid == toId attrs -> do
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
        (getIsPlayable iid [DuringTurn iid, NonFast])
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
    _ -> EverVigilant <$> runMessage msg attrs
