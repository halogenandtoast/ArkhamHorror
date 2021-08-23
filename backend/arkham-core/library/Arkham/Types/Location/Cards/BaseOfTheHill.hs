module Arkham.Types.Location.Cards.BaseOfTheHill
  ( baseOfTheHill
  , BaseOfTheHill(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window hiding (SuccessfulInvestigation)

newtype BaseOfTheHill = BaseOfTheHill LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseOfTheHill :: LocationCard BaseOfTheHill
baseOfTheHill = location
  BaseOfTheHill
  Cards.baseOfTheHill
  3
  (Static 0)
  Triangle
  [Square, Plus, Squiggle, Hourglass]

instance HasAbilities env BaseOfTheHill where
  getAbilities iid window@(Window Timing.When NonFast) (BaseOfTheHill attrs)
    | locationRevealed attrs = withResignAction iid window attrs $ pure
      [ restrictedAbility
            attrs
            1
            Here
            (ActionAbility (Just Action.Investigate) (ActionCost 1))
          & (abilityLimitL .~ PlayerLimit PerRound 1)
      ]
  getAbilities iid window (BaseOfTheHill attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env BaseOfTheHill where
  runMessage msg l@(BaseOfTheHill attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (Investigate
        iid
        (toId attrs)
        (AbilitySource source 1)
        SkillIntellect
        False
      )
    SuccessfulInvestigation _ _ (AbilitySource source 1)
      | isSource attrs source -> do
        setAsideCards <- map unSetAsideCard <$> getList @SetAsideCard ()
        let
          divergingPaths =
            filter ((== "Diverging Path") . toName) setAsideCards
        case nonEmpty divergingPaths of
          Just ne -> do
            card <- sample ne
            l <$ push
              (PlaceLocation (LocationId $ toCardId card) (toCardDef card))
          Nothing -> pure l
    AddConnection lid _ | toId attrs /= lid -> do
      isDivergingPath <- (== "Diverging Path") <$> getName lid
      if isDivergingPath
        then BaseOfTheHill
          <$> runMessage msg (attrs & connectedLocationsL %~ insertSet lid)
        else BaseOfTheHill <$> runMessage msg attrs
    _ -> BaseOfTheHill <$> runMessage msg attrs
