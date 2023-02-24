module Arkham.Event.Cards.Shortcut2
  ( shortcut2
  , Shortcut2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Source

newtype Shortcut2 = Shortcut2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortcut2 :: EventCard Shortcut2
shortcut2 = event Shortcut2 Cards.shortcut2

instance HasAbilities Shortcut2 where
  getAbilities (Shortcut2 a) = case eventAttachedTarget a of
    Just (LocationTarget lid) ->
      [ restrictedAbility
          (ProxySource (LocationSource lid) (toSource a))
          1
          (OnLocation (LocationWithId lid) <> LocationExists AccessibleLocation)
          (FastAbility $ ExhaustCost (toTarget a))
      ]
    _ -> []

instance RunMessage Shortcut2 where
  runMessage msg e@(Shortcut2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- selectJust $ LocationWithInvestigator $ InvestigatorWithId iid
      e <$ push (PlaceEvent iid eid (AttachedToLocation lid))
    UseCardAbility iid (ProxySource _ source) 1 _ _ | isSource attrs source -> do
      connectingLocations <- selectList $ AccessibleLocation
      push $ chooseOne
        iid
        [ targetLabel lid' [Move (toSource attrs) iid lid']
        | lid' <- connectingLocations
        ]
      pure e
    _ -> Shortcut2 <$> runMessage msg attrs
