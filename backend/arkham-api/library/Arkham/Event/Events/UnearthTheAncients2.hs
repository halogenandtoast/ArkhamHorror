module Arkham.Event.Events.UnearthTheAncients2 (unearthTheAncients2, UnearthTheAncients2 (..)) where

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

newtype Metadata = Metadata {chosenCards :: [CardId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UnearthTheAncients2 = UnearthTheAncients2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unearthTheAncients2 :: EventCard UnearthTheAncients2
unearthTheAncients2 = event (UnearthTheAncients2 . (`with` Metadata [])) Cards.unearthTheAncients2

-- Rules as written says that yes, you could commit the chosen cards to the
-- test, and put them into play when the test resolves. We may revisit this in
-- a future FAQ (not 2.0, which is about to be released).

instance RunMessage UnearthTheAncients2 where
  runMessage msg e@(UnearthTheAncients2 (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      assets <- select $ InHandOf (InvestigatorWithId iid) <> BasicCardMatch (#seeker <> #asset)
      player <- getPlayer iid
      pushAll
        [ chooseUpToN player 2 "Do not choose any more assets"
            $ [ targetLabel asset [HandleTargetChoice iid (toSource attrs) (toTarget asset)]
              | asset <- assets
              ]
        , ResolveEvent iid eid Nothing windows'
        ]
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (CardIdTarget cid) -> do
      pure $ UnearthTheAncients2 $ attrs `with` Metadata (cid : chosenCards metadata)
    ResolveEvent iid eid _ _ | eid == toId attrs -> do
      sid <- getRandom
      investigation <- mkInvestigate sid iid attrs
      cards <- traverse getCard (chosenCards metadata)
      enabled <- skillTestModifier sid attrs sid (SetDifficulty $ sum $ map getCost cards)
      pushAll [enabled, toMessage investigation]
      pure e
    Successful (Action.Investigate, _) iid (isSource attrs -> True) _ _ -> do
      cards <- traverse getCard (chosenCards metadata)
      let chosen = map (\card -> (card, drawCards iid attrs 1)) cards
      pushAll
        $ [putCardIntoPlay iid card | card <- cards]
        <> [drawing | (card, drawing) <- chosen, Relic `member` toTraits card]
      pure e
    _ -> UnearthTheAncients2 . (`with` metadata) <$> runMessage msg attrs
