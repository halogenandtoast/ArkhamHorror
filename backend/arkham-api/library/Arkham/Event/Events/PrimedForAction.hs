module Arkham.Event.Events.PrimedForAction (primedForAction) where

import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype PrimedForAction = PrimedForAction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primedForAction :: EventCard PrimedForAction
primedForAction = event PrimedForAction Cards.primedForAction

instance RunMessage PrimedForAction where
  runMessage msg e@(PrimedForAction attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      firearms <-
        select $ PlayableCardWithCostReduction NoAction 2 (inHandOf ForPlay attrs.owner <> basic #firearm)
      focusCards firearms do
        chooseOrRunOneM iid do
          targets firearms \firearm -> do
            reduceCostOf attrs firearm 2
            playCardPayingCost iid firearm
            push $ ForTarget (CardIdTarget firearm.id) msg
      pure e
    ForTarget (CardIdTarget cid) (PlayThisEvent iid (is attrs -> True)) -> do
      masset <- selectOne $ AssetWithCardId cid
      let
        playUpgrade :: ReverseQueue m => m ()
        playUpgrade = do
          upgrades <- select $ PlayableCard (UnpaidCost NoAction) (inHandOf ForPlay attrs.owner <> basic #upgrade)
          focusCards upgrades do
            chooseOrRunOneM iid do
              labeled "Do not play upgrade" nothing
              targets upgrades $ playCardPayingCost iid
      case masset of
        Nothing -> playUpgrade
        Just asset -> temporaryModifier iid (attrs.ability 1) (UpgradeTargetIfAble (toTarget asset)) playUpgrade
      pure e
    _ -> PrimedForAction <$> liftRunMessage msg attrs
