module Arkham.Types.Location.Cards.AscendingPath
  ( ascendingPath
  , AscendingPath(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source

newtype AscendingPath = AscendingPath LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingPath :: LocationCard AscendingPath
ascendingPath = location
  AscendingPath
  Cards.ascendingPath
  3
  (Static 0)
  Square
  [Triangle, Diamond, T, Equals, Moon]

instance HasModifiersFor env AscendingPath where
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

instance LocationRunner env => RunMessage env AscendingPath where
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
    SuccessfulInvestigation _ _ (AbilitySource source 1) _
      | isSource attrs source -> do
        alteredPaths <- getSetAsideCardsMatching $ CardWithTitle "Altered Path"
        case nonEmpty alteredPaths of
          Just ne -> do
            card <- sample ne
            l <$ push (PlaceLocation card)
          Nothing -> pure l
    AddConnection lid _ | toId attrs /= lid -> do
      isAlteredPath <- (== "Altered Path") <$> getName lid
      if isAlteredPath
        then AscendingPath
          <$> runMessage msg (attrs & connectedLocationsL %~ insertSet lid)
        else AscendingPath <$> runMessage msg attrs
    _ -> AscendingPath <$> runMessage msg attrs
