module Arkham.Event.Events.HoldUp (holdUp) where

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Playable (getIsPlayableWithResources)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Taboo
import Arkham.Window (defaultWindows)

newtype Meta = Meta {chosenCard :: Maybe CardId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HoldUp = HoldUp (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holdUp :: EventCard HoldUp
holdUp = event (HoldUp . (`with` Meta Nothing)) Cards.holdUp

handleSuccess :: ReverseQueue m => InvestigatorId -> HoldUp -> Int -> m ()
handleSuccess iid (HoldUp (attrs `With` meta)) n = do
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

instance RunMessage HoldUp where
  runMessage msg e@(HoldUp (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ inHandOf NotForPlay iid <> #item <> #asset
      sid <- getRandom
      if tabooed TabooList24 attrs
        then chooseFightEnemy sid iid attrs
        else chooseFightEnemyEdit sid iid attrs (setTarget attrs)
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (CardIdTarget cid) -> do
      pure . HoldUp $ attrs `with` Meta (Just cid)
    Successful (Action.Fight, EnemyTarget _eid) iid _ (isTarget attrs -> True) n -> do
      handleSuccess iid e n
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      handleSuccess iid e n
      pure e
    _ -> HoldUp . (`with` meta) <$> liftRunMessage msg attrs
