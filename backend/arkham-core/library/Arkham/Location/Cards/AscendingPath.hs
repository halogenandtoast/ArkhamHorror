module Arkham.Location.Cards.AscendingPath
  ( ascendingPath
  , AscendingPath(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source

newtype AscendingPath = AscendingPath LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingPath :: LocationCard AscendingPath
ascendingPath = locationWith
  AscendingPath
  Cards.ascendingPath
  3
  (Static 0)
  Square
  [Triangle, Diamond, T, Equals, Moon]
  (revealedConnectedMatchersL <>~ [LocationWithTitle "Altered Path"])

instance HasModifiersFor AscendingPath where
  getModifiersFor _ target (AscendingPath l@LocationAttrs {..})
    | isTarget l target = pure
    $ toModifiers l [ Blocked | not locationRevealed ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AscendingPath where
  getAbilities (AscendingPath attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
              attrs
              1
              Here
              (ActionAbility (Just Action.Investigate) (ActionCost 1))
            & (abilityLimitL .~ PlayerLimit PerRound 1)
        | locationRevealed attrs
        ]

instance LocationRunner env => RunMessage AscendingPath where
  runMessage msg l@(AscendingPath attrs) = case msg of
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
        alteredPaths <- getSetAsideCardsMatching $ CardWithTitle "Altered Path"
        case nonEmpty alteredPaths of
          Just ne -> do
            card <- sample ne
            l <$ push (PlaceLocation card)
          Nothing -> pure l
    _ -> AscendingPath <$> runMessage msg attrs
