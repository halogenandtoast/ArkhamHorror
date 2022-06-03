module Arkham.Location.Cards.BaseOfTheHill
  ( baseOfTheHill
  , BaseOfTheHill(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source

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

instance LocationRunner env => RunMessage BaseOfTheHill where
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
    Successful (Action.Investigate, _) _ (AbilitySource source 1) _ _
      | isSource attrs source -> do
        divergingPaths <- getSetAsideCardsMatching
          $ CardWithTitle "Diverging Path"
        case nonEmpty divergingPaths of
          Just ne -> do
            card <- sample ne
            l <$ push (PlaceLocation card)
          Nothing -> pure l
    _ -> BaseOfTheHill <$> runMessage msg attrs
