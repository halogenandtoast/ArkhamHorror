module Arkham.Event.Events.EverVigilant1 (everVigilant1, EverVigilant1 (..)) where

import Arkham.Card
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getIsPlayable)
import Arkham.Helpers.Modifiers (ModifierType (..), toModifiers, withModifiers)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Window

newtype EverVigilant1 = EverVigilant1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everVigilant1 :: EventCard EverVigilant1
everVigilant1 = event EverVigilant1 Cards.everVigilant1

instance RunMessage EverVigilant1 where
  runMessage msg e@(EverVigilant1 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid mtarget windows' _ | eid == attrs.id -> do
      pushAll $ replicate 3 (ResolveEvent iid eid mtarget windows')
      pure e
    ResolveEvent iid eid _mtarget windows' | eid == toId attrs -> do
      let windows'' = nub $ windows' <> [mkWhen (DuringTurn iid), mkWhen NonFast]
      cards <- fieldMap InvestigatorHand (filter (`cardMatch` CardWithType AssetType)) iid
      playableCards <- withModifiers iid (toModifiers attrs [ReduceCostOf AnyCard 1]) do
        filterM (getIsPlayable iid GameSource (UnpaidCost NoAction) windows'') cards
      when (notNull playableCards) do
        chooseUpToNM iid 1 "Do not play asset" do
          targets playableCards \c -> do
            costModifier attrs c $ ReduceCostOf AnyCard 1
            push $ PayCardCost iid c windows''
      pure e
    _ -> EverVigilant1 <$> liftRunMessage msg attrs
