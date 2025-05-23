module Arkham.Event.Events.Teamwork (teamwork) where

import Arkham.Classes.HasQueue (HasQueue)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype Teamwork = Teamwork EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

teamwork :: EventCard Teamwork
teamwork = event Teamwork Cards.teamwork

instance RunMessage Teamwork where
  runMessage msg e@(Teamwork attrs) = runQueueT $ case msg of
    PlayThisEvent _ (is attrs -> True) -> do
      do_ msg
      pure e
    Do msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      investigators <- select $ colocatedWith iid
      assetsWithInvestigator <- concatForM investigators \investigator -> do
        selectMap (investigator,) $ assetControlledBy investigator <> mapOneOf AssetWithTrait [Ally, Item]
      let
        beginTrade :: (Targetable target, HasQueue Message m) => InvestigatorId -> target -> m ()
        beginTrade iid' x = push $ BeginTrade iid' (toSource attrs) (toTarget x) (investigators \\ [iid'])
      let resolveAgain = do_ msg'

      chooseOneM iid do
        labeled "Done Trading" nothing
        for_ assetsWithInvestigator \(iid', aid) -> do
          targeting aid do
            beginTrade iid' aid
            resolveAgain
        targets investigators \iid' -> do
          beginTrade iid' (ResourceTarget iid)
          resolveAgain
      pure e
    _ -> Teamwork <$> liftRunMessage msg attrs
