module Arkham.Event.Cards.Shortcut (
  shortcut,
  Shortcut (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Movement

newtype Shortcut = Shortcut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortcut :: EventCard Shortcut
shortcut = event Shortcut Cards.shortcut

instance RunMessage Shortcut where
  runMessage msg e@(Shortcut attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      investigatorIds <- selectList $ colocatedWith iid
      connectingLocations <- selectList AccessibleLocation
      player <- getPlayer iid
      unless (null connectingLocations)
        $ pushAll
          [ chooseOrRunOne
              player
              [ TargetLabel
                (InvestigatorTarget iid')
                [ chooseOne
                    player
                    [ TargetLabel
                      (LocationTarget lid')
                      [Move $ move (toSource attrs) iid' lid']
                    | lid' <- connectingLocations
                    ]
                ]
              | iid' <- investigatorIds
              ]
          ]
      pure e
    _ -> Shortcut <$> runMessage msg attrs
