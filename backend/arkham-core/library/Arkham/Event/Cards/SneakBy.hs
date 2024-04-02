module Arkham.Event.Cards.SneakBy (sneakBy, SneakBy (..)) where

import Arkham.Classes
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Prelude

newtype SneakBy = SneakBy EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sneakBy :: EventCard SneakBy
sneakBy = event SneakBy Cards.sneakBy

instance RunMessage SneakBy where
  runMessage msg e@(SneakBy attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      chooseEvade <- toMessage <$> mkChooseEvade iid attrs
      pushAll
        [ TakeResources iid 2 (toSource attrs) False
        , chooseEvade
        ]
      pure e
    _ -> SneakBy <$> runMessage msg attrs
