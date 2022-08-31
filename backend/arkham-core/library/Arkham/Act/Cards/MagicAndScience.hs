module Arkham.Act.Cards.MagicAndScience
  ( MagicAndScience(..)
  , magicAndScience
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Label
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message hiding ( Label )
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Target
import Control.Monad.Extra ( findM )
import Data.Function ( on )
import Data.Monoid ( Last (..) )

newtype MagicAndScience = MagicAndScience ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magicAndScience :: ActCard MagicAndScience
magicAndScience = act
  (2, A)
  MagicAndScience
  Cards.magicAndScience
  (Just $ GroupClueCost (PerPlayer 2) (LocationWithTitle "Chamber of Time"))

instance HasAbilities MagicAndScience where
  getAbilities (MagicAndScience a) = withBaseAbilities
    a
    [ restrictedAbility a 1 (ScenarioDeckWithCard ExplorationDeck)
      $ ActionAbility (Just Action.Explore)
      $ ActionCost 1
    ]

data LocationCandidate = LocationCandidate
  { locationCandidateId :: LocationId
  , locationCandidateDoom :: Int
  , locationCandidateCard :: Card
  }

instance RunMessage MagicAndScience where
  runMessage msg a@(MagicAndScience attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore
        iid
        source
        (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      chamberOfTime <- selectJust $ locationIs Locations.chamberOfTime
      entryway <- selectJust $ locationIs Locations.entryway
      otherLocations <- selectList $ NotLocation $ LocationMatchAny
        [LocationWithId chamberOfTime, LocationWithId entryway]
      candidates <- for otherLocations $ \location -> do
        LocationCandidate location
          <$> field LocationDoom location
          <*> field LocationCard location
      let
        candidateGroups = groupBy ((==) `on` locationCandidateDoom)
          $ sortBy (compare `on` locationCandidateDoom) candidates
        handleCandidateGroup [] = error "Unhandled"
        handleCandidateGroup [c] =
          [ HandleTargetChoice
              leadInvestigatorId
              (toSource attrs)
              (LocationTarget $ locationCandidateId c)
          ]
        handleCandidateGroup cs =
          [ FocusCards $ map locationCandidateCard cs
          , chooseOneAtATime
            leadInvestigatorId
            [ TargetLabel
                (CardIdTarget $ toCardId $ locationCandidateCard c)
                [ HandleTargetChoice
                    leadInvestigatorId
                    (toSource attrs)
                    (LocationTarget $ locationCandidateId c)
                ]
            | c <- cs
            ]
          , UnfocusCards
          ]
      pushAll
        $ [ SetLocationLabel chamberOfTime "pos1"
          , SetLocationLabel entryway "pos7"
          ]
        <> concatMap handleCandidateGroup candidateGroups
        <> [ NextAdvanceActStep (toId attrs) 1
           , AdvanceActDeck (actDeckId attrs) (toSource attrs)
           ]
      pure a
    HandleTargetChoice _ source (LocationTarget lid) | isSource attrs source ->
      do
        -- find first unfilled position
        let
          toPosition = ("pos" <>) . tshow @Int
          positions = map (toFst toPosition) [2 .. 6]
        mpositition <- findM
          (selectNone . LocationWithLabel . Label . fst)
          positions
        case mpositition of
          Nothing -> error "invalid logic"
          Just (pos, n) -> do
            let left = toPosition (n - 1)
            leftLocation <- selectJust $ LocationWithLabel $ Label left
            pushAll
              [ SetLocationLabel lid pos
              , PlacedLocationDirection lid RightOf leftLocation
              ]
            pure a
    NextAdvanceActStep actId 1 | toId attrs == actId -> do
      -- find last filled position
      let
        toPosition = ("pos" <>) . tshow @Int
        positions = map toPosition [2 .. 6]
      mposition <-
        getLast
        . foldMap Last
        <$> traverse (selectOne . LocationWithLabel . Label) positions
      case mposition of
        Nothing -> error "invalid logic"
        Just pos -> do
          entryway <- selectJust $ locationIs Locations.entryway
          push $ PlacedLocationDirection entryway RightOf pos
          pure a
    _ -> MagicAndScience <$> runMessage msg attrs
