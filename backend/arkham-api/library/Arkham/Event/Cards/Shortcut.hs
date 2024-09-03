module Arkham.Event.Cards.Shortcut (shortcut, Shortcut (..)) where

import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude

newtype Shortcut = Shortcut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortcut :: EventCard Shortcut
shortcut = event Shortcut Cards.shortcut

instance RunMessage Shortcut where
  runMessage msg e@(Shortcut attrs@EventAttrs {..}) = case msg of
    PlayThisEvent iid eid | eid == eventId -> do
      investigatorIds <- select =<< guardAffectsOthers iid (can.move <> colocatedWith iid)
      connectingLocations <- getAccessibleLocations iid attrs
      player <- getPlayer iid
      unless (null connectingLocations) do
        push
          $ chooseOrRunOne
            player
            [ targetLabel
              iid'
              [ chooseOne
                  player
                  [ targetLabel lid' [Move $ move (toSource attrs) iid' lid']
                  | lid' <- connectingLocations
                  ]
              ]
            | iid' <- investigatorIds
            ]
      pure e
    _ -> Shortcut <$> runMessage msg attrs
