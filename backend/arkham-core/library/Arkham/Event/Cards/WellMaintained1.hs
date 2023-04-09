module Arkham.Event.Cards.WellMaintained1
  ( wellMaintained1
  , WellMaintained1(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Cost
import Arkham.Criteria
import Arkham.Ability
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait(Item, Upgrade))

newtype WellMaintained1 = WellMaintained1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellMaintained1 :: EventCard WellMaintained1
wellMaintained1 = event WellMaintained1 Cards.wellMaintained1

instance HasAbilities WellMaintained1 where
  getAbilities (WellMaintained1 a) = case eventPlacement a of
    AttachedToAsset aid _ -> [restrictedAbility a 1 ControlsThis $ ReactionAbility (AssetWouldBeDiscarded Timing.After $ AssetWithId aid) Free]
    _ -> []

instance RunMessage WellMaintained1 where
  runMessage msg e@(WellMaintained1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <- selectList $ assetControlledBy iid <> AssetWithTrait Item
      push $ chooseOne
        iid
        [ targetLabel asset [PlaceEvent iid eid $ AttachedToAsset asset Nothing]
        | asset <- assets
        ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      case eventPlacement attrs of
        AttachedToAsset aid _ -> do
          otherUpgrades <- selectList $ EventAttachedToAsset (AssetWithId aid) <> NotEvent (EventWithId (toId attrs)) <> EventWithTrait Upgrade
          pushAll $ [ReturnToHand iid (toTarget upgrade) | upgrade <- otherUpgrades] <> [ReturnToHand iid (toTarget aid)]
        _ -> error "Invalid placement"
      pure e
    _ -> WellMaintained1 <$> runMessage msg attrs
