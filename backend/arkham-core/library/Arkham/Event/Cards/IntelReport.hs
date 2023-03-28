module Arkham.Event.Cards.IntelReport
  ( intelReport
  , IntelReport(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message hiding ( PlayCard )
import Arkham.Timing qualified as Timing

data Metadata = Metadata
  { clueCount :: Int
  , discoverUpToTwoAway :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype IntelReport = IntelReport (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intelReport :: EventCard IntelReport
intelReport = event (IntelReport . (`with` Metadata 1 False)) Cards.intelReport

instance HasAbilities IntelReport where
  getAbilities (IntelReport (a `With` _)) =
    [ withTooltip
        "{reaction} When you play Intel Report, increase its cost by 2: Change \"Discover 1 clue\" to \"Discover 2 clues.\""
      $ restrictedAbility a 1 InYourHand
      $ ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    , withTooltip
        "{reaction} When you play Intel Report, increase its cost by 2: Change \"at your location\" to \"at a location up to 2 connections away.\""
      $ restrictedAbility a 2 InYourHand
      $ ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch $ CardWithId $ toCardId a))
          (IncreaseCostOfThis (toCardId a) 2)
    ]

instance RunMessage IntelReport where
  runMessage msg e@(IntelReport (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      if discoverUpToTwoAway meta
        then do
          lids <- selectList $ LocationWithAnyClues <> LocationMatchAny
            [ LocationWithDistanceFrom n (locationWithInvestigator iid)
            | n <- [0 .. 2]
            ]
          push $ chooseOrRunOne
            iid
            [ targetLabel
                lid
                [InvestigatorDiscoverClues iid lid (clueCount meta) Nothing]
            | lid <- lids
            ]
        else push $ InvestigatorDiscoverCluesAtTheirLocation
          iid
          (clueCount meta)
          Nothing
      pure e
    InHand _ (UseCardAbility _ (isSource attrs -> True) 1 _ _) -> do
      pure . IntelReport $ attrs `with` meta { clueCount = 2 }
    InHand _ (UseCardAbility _ (isSource attrs -> True) 2 _ _) -> do
      pure . IntelReport $ attrs `with` meta { discoverUpToTwoAway = True }
    _ -> IntelReport . (`with` meta) <$> runMessage msg attrs
