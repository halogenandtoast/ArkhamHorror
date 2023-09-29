module Arkham.Event.Cards.UnearthTheAncients (
  unearthTheAncients,
  UnearthTheAncients (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait
import Arkham.Window (defaultWindows)

newtype Metadata = Metadata {chosenCard :: Maybe Card}
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
      assets <- selectList $ InHandOf (InvestigatorWithId iid) <> BasicCardMatch (#seeker <> #asset)
      push
        $ chooseOne iid
        $ [ targetLabel (toCardId asset) [ResolveEvent iid eid (Just $ CardTarget asset) windows']
          | asset <- assets
          ]
      pure e
    ResolveEvent iid eid (Just (CardTarget card)) _ | eid == toId attrs -> do
      investigation <- mkInvestigate iid attrs <&> setTarget attrs
      pushAll
        [ skillTestModifier attrs SkillTestTarget (SetDifficulty $ getCost card)
        , toMessage investigation
        ]
      pure $ UnearthTheAncients $ attrs `with` Metadata (Just card)
    Successful (Action.Investigate, _) iid _ target _ | isTarget attrs target -> do
      case chosenCard metadata of
        Just card -> do
          drawing <- drawCards iid attrs 1
          pushAll
            $ PutCardIntoPlay iid card Nothing (defaultWindows iid)
            : [drawing | Relic `member` toTraits card]
        Nothing -> error "this should not happen"
      pure e
    _ -> UnearthTheAncients . (`with` metadata) <$> runMessage msg attrs
