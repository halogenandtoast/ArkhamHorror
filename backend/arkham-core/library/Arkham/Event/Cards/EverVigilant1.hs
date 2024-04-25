module Arkham.Event.Cards.EverVigilant1 (
  everVigilant1,
  EverVigilant1 (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Window

newtype EverVigilant1 = EverVigilant1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everVigilant1 :: EventCard EverVigilant1
everVigilant1 = event EverVigilant1 Cards.everVigilant1

instance HasModifiersFor EverVigilant1 where
  getModifiersFor (InvestigatorTarget iid) (EverVigilant1 attrs) | iid == eventOwner attrs = do
    pure $ toModifiers attrs [ReduceCostOf AnyCard 1]
  getModifiersFor _ _ = pure []

instance RunMessage EverVigilant1 where
  runMessage msg e@(EverVigilant1 attrs) = case msg of
    InvestigatorPlayEvent iid eid mtarget windows' _ | eid == toId attrs -> do
      pushAll $ replicate 3 (ResolveEvent iid eid mtarget windows')
      pure e
    ResolveEvent iid eid _mtarget windows' | eid == toId attrs -> do
      let windows'' = nub $ windows' <> [mkWhen (DuringTurn iid), mkWhen NonFast]
      cards <- fieldMap InvestigatorHand (filter (`cardMatch` CardWithType AssetType)) iid
      playableCards <- filterM (getIsPlayable iid GameSource (UnpaidCost NoAction) windows'') cards
      player <- getPlayer iid
      pushWhen (notNull playableCards)
        $ chooseUpToN
          player
          1
          "Do not play asset"
          [targetLabel (toCardId c) [PayCardCost iid c windows''] | c <- playableCards]
      pure e
    _ -> EverVigilant1 <$> runMessage msg attrs
