module Arkham.Event.Cards.HoldUp (holdUp, HoldUp (..)) where

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getIsPlayableWithResources)
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype Meta = Meta {chosenCard :: Maybe CardId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HoldUp = HoldUp (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holdUp :: EventCard HoldUp
holdUp = event (HoldUp . (`with` Meta Nothing)) Cards.holdUp

instance RunMessage HoldUp where
  runMessage msg e@(HoldUp (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ inHandOf iid <> #item <> #asset
      sid <- getRandom
      chooseFightEnemyEdit sid iid attrs (setTarget attrs)
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (CardIdTarget cid) -> do
      pure . HoldUp $ attrs `with` Meta (Just cid)
    Successful (Action.Fight, EnemyTarget _eid) iid _ (isTarget attrs -> True) n -> do
      for_ (chosenCard meta) \cid -> do
        card <- getCard cid
        resources <- field InvestigatorResources iid
        playable <-
          getIsPlayableWithResources
            iid
            GameSource
            (resources + n)
            (UnpaidCost NoAction)
            (defaultWindows iid)
            card
        when playable do
          costModifier attrs iid $ ReduceCostOf (CardWithId cid) n
          payCardCost iid card

      pure e
    _ -> HoldUp . (`with` meta) <$> liftRunMessage msg attrs
