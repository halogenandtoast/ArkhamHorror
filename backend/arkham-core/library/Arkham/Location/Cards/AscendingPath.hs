module Arkham.Location.Cards.AscendingPath (
  ascendingPath,
  AscendingPath (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype AscendingPath = AscendingPath LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingPath :: LocationCard AscendingPath
ascendingPath =
  locationWith
    AscendingPath
    Cards.ascendingPath
    3
    (Static 0)
    (revealedConnectedMatchersL <>~ [LocationWithTitle "Altered Path"])

instance HasModifiersFor AscendingPath where
  getModifiersFor target (AscendingPath l@LocationAttrs {..})
    | isTarget l target =
        pure
          $ toModifiers l [Blocked | not locationRevealed]
  getModifiersFor _ _ = pure []

instance HasAbilities AscendingPath where
  getAbilities (AscendingPath attrs) =
    withBaseAbilities attrs
      $ [ withTooltip
          "{action}: _Investigate_. If you succeed, instead of discovering clues, put a random set-aside Altered Path into play. (Limit once per round.)"
          $ limitedAbility (PlayerLimit PerRound 1)
          $ restrictedAbility
            attrs
            1
            Here
            (ActionAbility (Just Action.Investigate) (ActionCost 1))
        | locationRevealed attrs
        ]

instance RunMessage AscendingPath where
  runMessage msg l@(AscendingPath attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l
            <$ push
              ( Investigate
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
              placement <- placeLocation_ card
              l <$ push placement
            Nothing -> pure l
    _ -> AscendingPath <$> runMessage msg attrs
