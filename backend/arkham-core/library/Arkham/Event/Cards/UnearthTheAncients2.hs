module Arkham.Event.Cards.UnearthTheAncients2 (
  unearthTheAncients2,
  UnearthTheAncients2 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Trait
import Arkham.Window (defaultWindows)

newtype Metadata = Metadata {chosenCards :: [Card]}
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
            $ [ targetLabel (toCardId asset) [HandleTargetChoice iid (toSource attrs) (CardTarget asset)]
              | asset <- assets
              ]
        , ResolveEvent iid eid Nothing windows'
        ]
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (CardTarget card) -> do
      pure $ UnearthTheAncients2 $ attrs `with` Metadata (card : chosenCards metadata)
    ResolveEvent iid eid _ _ | eid == toId attrs -> do
      investigation <- mkInvestigate iid attrs
      pushAll
        [ skillTestModifier attrs SkillTestTarget (SetDifficulty $ sum $ map getCost $ chosenCards metadata)
        , toMessage investigation
        ]
      pure e
    Successful (Action.Investigate, _) iid (isSource attrs -> True) _ _ -> do
      chosen <- forToSnd (chosenCards metadata) $ \_ -> drawCards iid attrs 1
      pushAll
        $ [putCardIntoPlay iid card | card <- chosenCards metadata]
        <> [drawing | (card, drawing) <- chosen, Relic `member` toTraits card]
      pure e
    _ -> UnearthTheAncients2 . (`with` metadata) <$> runMessage msg attrs
