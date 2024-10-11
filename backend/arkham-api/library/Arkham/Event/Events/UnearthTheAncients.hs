module Arkham.Event.Events.UnearthTheAncients (unearthTheAncients, UnearthTheAncients (..)) where

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype Metadata = Metadata {chosenCard :: Maybe CardId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UnearthTheAncients = UnearthTheAncients (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unearthTheAncients :: EventCard UnearthTheAncients
unearthTheAncients = event (UnearthTheAncients . (`with` Metadata Nothing)) Cards.unearthTheAncients

-- Rules as written says that yes, you could commit the chosen cards to the
-- test, and put them into play when the test resolves. We may revisit this in
-- a future FAQ (not 2.0, which is about to be released).

instance RunMessage UnearthTheAncients where
  runMessage msg e@(UnearthTheAncients (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      assets <- select $ inHandOf iid <> basic (#seeker <> #asset)
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ targetLabel asset [ResolveEvent iid eid (Just $ toTarget asset) windows']
          | asset <- assets
          ]
      pure e
    ResolveEvent iid eid (Just (CardIdTarget cid)) _ | eid == toId attrs -> do
      sid <- getRandom
      investigation <- mkInvestigate sid iid attrs <&> setTarget attrs
      card <- getCard cid
      enabled <- skillTestModifier sid attrs sid (SetDifficulty $ getCost card)
      pushAll [enabled, toMessage investigation]
      pure $ UnearthTheAncients $ attrs `with` Metadata (Just cid)
    Successful (Action.Investigate, _) iid _ target _ | isTarget attrs target -> do
      case chosenCard metadata of
        Just cid -> do
          card <- getCard cid
          let drawing = drawCards iid attrs 1
          pushAll $ putCardIntoPlay iid card : [drawing | Relic `member` toTraits card]
        Nothing -> error "this should not happen"
      pure e
    _ -> UnearthTheAncients . (`with` metadata) <$> runMessage msg attrs
