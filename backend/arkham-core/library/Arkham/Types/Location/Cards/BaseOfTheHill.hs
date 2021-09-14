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
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Source

newtype BaseOfTheHill = BaseOfTheHill LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseOfTheHill :: LocationCard BaseOfTheHill
baseOfTheHill = locationWith
  BaseOfTheHill
  Cards.baseOfTheHill
  3
  (Static 0)
  Triangle
  [Square, Plus, Squiggle, Hourglass]
  (revealedConnectedMatchersL <>~ [LocationWithTitle "Diverging Path"])

instance HasAbilities BaseOfTheHill where
  getAbilities (BaseOfTheHill attrs) = withResignAction
    attrs
    [ restrictedAbility
          attrs
          1
          Here
          (ActionAbility (Just Action.Investigate) (ActionCost 1))
        & (abilityLimitL .~ PlayerLimit PerRound 1)
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env BaseOfTheHill where
  runMessage msg l@(BaseOfTheHill attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (Investigate
        iid
        (toId attrs)
        (AbilitySource source 1)
        Nothing
        SkillIntellect
        False
      )
    SuccessfulInvestigation _ _ (AbilitySource source 1) _
      | isSource attrs source -> do
        setAsideCards <- map unSetAsideCard <$> getList @SetAsideCard ()
        let
          divergingPaths =
            filter ((== "Diverging Path") . toName) setAsideCards
        case nonEmpty divergingPaths of
          Just ne -> do
            card <- sample ne
            l <$ push (PlaceLocation card)
          Nothing -> pure l
    _ -> BaseOfTheHill <$> runMessage msg attrs
