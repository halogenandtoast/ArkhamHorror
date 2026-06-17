module Arkham.Event.Events.SeekingAnswers2 (seekingAnswers2, SeekingAnswers2 (..)) where

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.ForMovement
import Arkham.Helpers.Investigator (getCanDiscoverClues)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype SeekingAnswers2 = SeekingAnswers2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers2 :: EventCard SeekingAnswers2
seekingAnswers2 = event SeekingAnswers2 Cards.seekingAnswers2

instance RunMessage SeekingAnswers2 where
  runMessage msg e@(SeekingAnswers2 attrs@EventAttrs {..}) = case msg of
    PlayThisEvent iid eid | eid == eventId -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid attrs <&> setTarget attrs
      pure e
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      pushAll
        [ ResolveEvent iid (toId attrs) Nothing []
        , ResolveEvent iid (toId attrs) Nothing []
        ]
      -- "discover an additional clue at that location" effects (Deduction/Rex/etc.) resolve
      -- once at the investigated location, not at the redirected connecting locations.
      whenJustM (getLocationOf iid) \lid -> do
        canDiscover <- getCanDiscoverClues IsInvestigate iid lid
        when canDiscover do
          did <- getRandom
          push $ Msg.DiscoverClues iid $ viaInvestigate $ discoverPure did lid (toSource attrs) 0
      pure e
    ResolveEvent iid eid _ _ | eid == toId attrs -> do
      lids <-
        select
          $ oneOf [locationWithInvestigator iid, ConnectedLocation NotForMovement]
          <> locationWithDiscoverableCluesBy iid
      player <- getPlayer iid
      did <- getRandom
      pushIfAny lids
        $ chooseOrRunOne player
        $ [ targetLabel lid'
              $ [Msg.DiscoverClues iid $ discoverPure did lid' (toSource attrs) 1]
          | lid' <- lids
          ]
      pure e
    _ -> SeekingAnswers2 <$> runMessage msg attrs
