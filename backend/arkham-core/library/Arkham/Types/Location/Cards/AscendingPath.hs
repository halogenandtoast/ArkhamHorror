module Arkham.Types.Location.Cards.AscendingPath
  ( ascendingPath
  , AscendingPath(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Window

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

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility
      (toSource attrs)
      1
      (ActionAbility (Just Action.Investigate) (ActionCost 1))
    & (abilityLimitL .~ PlayerLimit PerRound 1)

instance ActionRunner env => HasActions env AscendingPath where
  getActions iid NonFast (AscendingPath attrs) =
    withBaseActions iid NonFast attrs
      $ pure [ locationAbility iid (ability attrs) | locationRevealed attrs ]
  getActions iid window (AscendingPath attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env AscendingPath where
  runMessage msg l@(AscendingPath attrs) = case msg of
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
        setAsideCards <- map unSetAsideCard <$> getList ()
        let alteredPaths = filter ((== "Altered Path") . toName) setAsideCards
        case nonEmpty alteredPaths of
          Just ne -> do
            card <- sample ne
            l <$ push
              (PlaceLocation (LocationId $ toCardId card) (toCardDef card))
          Nothing -> pure l
    AddConnection lid _ | toId attrs /= lid -> do
      isAlteredPath <- (== "Altered Path") <$> getName lid
      if isAlteredPath
        then AscendingPath
          <$> runMessage msg (attrs & connectedLocationsL %~ insertSet lid)
        else AscendingPath <$> runMessage msg attrs
    _ -> AscendingPath <$> runMessage msg attrs
