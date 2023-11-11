module Arkham.Event.Cards.FaustianBargain (
  faustianBargain,
  FaustianBargain (..),
)
where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype FaustianBargain = FaustianBargain EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

faustianBargain :: EventCard FaustianBargain
faustianBargain = event FaustianBargain Cards.faustianBargain

instance RunMessage FaustianBargain where
  runMessage msg e@(FaustianBargain attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      lid <- getJustLocation iid
      iids <- selectList $ affectsOthers $ investigatorAt lid <> can.gain.resources
      player <- getPlayer iid
      pushAll
        $ replicate 5
        $ chooseOrRunOne player [ResourceLabel iid' [takeResources iid' attrs 1] | iid' <- iids]
      pure e
    _ -> FaustianBargain <$> runMessage msg attrs
