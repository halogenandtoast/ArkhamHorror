module Arkham.Types.Location.Cards.AscendingPath
  ( ascendingPath
  , AscendingPath(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (ascendingPath)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Window

newtype AscendingPath = AscendingPath LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingPath :: LocationId -> AscendingPath
ascendingPath = AscendingPath . baseAttrs
  Cards.ascendingPath
  3
  (Static 0)
  Square
  [Triangle, Diamond, T, Equals, Moon]

instance HasModifiersFor env AscendingPath where
  getModifiersFor _ target (AscendingPath location@LocationAttrs {..})
    | isTarget location target = pure
    $ toModifiers location [ Blocked | not locationRevealed ]
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility
      (toSource attrs)
      1
      (ActionAbility (Just Action.Investigate) (ActionCost 1))
    & (abilityLimitL .~ PlayerLimit PerRound 1)

instance ActionRunner env => HasActions env AscendingPath where
  getActions iid NonFast (AscendingPath attrs) | iid `on` attrs =
    withBaseActions iid NonFast attrs
      $ pure
          [ ActivateCardAbilityAction iid (ability attrs)
          | locationRevealed attrs
          ]
  getActions iid window (AscendingPath attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env AscendingPath where
  runMessage msg l@(AscendingPath attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (Investigate
          iid
          (toId attrs)
          (AbilitySource source 1)
          SkillIntellect
          False
        )
    SuccessfulInvestigation _ _ (AbilitySource source 1)
      | isSource attrs source -> do
        locations <- getSetList @SetAsideLocationCardCode ()
        alteredPaths <- filterM
          (fmap (== "Altered Path") . getName)
          locations
        case nonEmpty alteredPaths of
          Just ne -> do
            newLocationId <- getRandom
            l
              <$ (unshiftMessage
                 . (`PlaceLocation` newLocationId)
                 . unSetAsideLocationCardCode
                 =<< sample ne
                 )
          Nothing -> pure l
    AddConnection lid _ | toId attrs /= lid -> do
      isAlteredPath <- (== "Altered Path") <$> getName lid
      if isAlteredPath
        then AscendingPath
          <$> runMessage msg (attrs & connectedLocationsL %~ insertSet lid)
        else AscendingPath <$> runMessage msg attrs
    _ -> AscendingPath <$> runMessage msg attrs
