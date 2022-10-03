module Arkham.Location.Cards.BaseOfTheHill
  ( baseOfTheHill
  , BaseOfTheHill(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source

newtype BaseOfTheHill = BaseOfTheHill LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseOfTheHill :: LocationCard BaseOfTheHill
baseOfTheHill = locationWith
  BaseOfTheHill
  Cards.baseOfTheHill
  3
  (Static 0)
  (revealedConnectedMatchersL <>~ [LocationWithTitle "Diverging Path"])

instance HasAbilities BaseOfTheHill where
  getAbilities (BaseOfTheHill attrs) = withResignAction
    attrs
    [ withTooltip
        "{action}: _Investigate_. If you succeed, instead of discovering clues, put a random set-aside Diverging Path into play. (Limit once per round.)"
      $ limitedAbility (PlayerLimit PerRound 1)
      $ restrictedAbility
          attrs
          1
          Here
          (ActionAbility (Just Action.Investigate) (ActionCost 1))
    | locationRevealed attrs
    ]

instance RunMessage BaseOfTheHill where
  runMessage msg l@(BaseOfTheHill attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ push
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
