module Arkham.Event.Cards.UnearthTheAncients
  ( unearthTheAncients
  , UnearthTheAncients(..)
  ) where

import Arkham.Prelude

import Arkham.ClassSymbol
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait
import Arkham.Window (defaultWindows)

newtype Metadata = Metadata { chosenCard :: Maybe Card }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UnearthTheAncients = UnearthTheAncients (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unearthTheAncients :: EventCard UnearthTheAncients
unearthTheAncients = event
  (UnearthTheAncients . (`with` Metadata Nothing))
  Cards.unearthTheAncients

instance RunMessage UnearthTheAncients where
  runMessage msg e@(UnearthTheAncients (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      assets <- selectList $ InHandOf (InvestigatorWithId iid) <> BasicCardMatch
        (CardWithClass Seeker <> CardWithType AssetType)
      push $ chooseOne
        iid
        [ TargetLabel
            (CardTarget asset)
            [ResolveEvent iid eid (Just $ CardTarget asset) windows']
        | asset <- assets
        ]
      pure e
    ResolveEvent iid eid (Just (CardTarget card)) _ | eid == toId attrs -> do
      lid <- getJustLocation iid
      pushAll
        [ Investigate
          iid
          lid
          (toSource attrs)
          (Just $ toTarget attrs)
          SkillIntellect
          False
        , Discard (toTarget attrs)
        ]
      pure $ UnearthTheAncients $ attrs `with` Metadata (Just card)
    Successful (Action.Investigate, _) iid _ target _ | isTarget attrs target ->
      do
        case chosenCard metadata of
          Just card ->
            pushAll
              $ PutCardIntoPlay iid card Nothing (defaultWindows iid)
              : [ DrawCards iid 1 False
                 | Relic `member` cdCardTraits (toCardDef card)
                 ]
          Nothing -> error "this should not happen"
        pure e
    _ -> UnearthTheAncients . (`with` metadata) <$> runMessage msg attrs
