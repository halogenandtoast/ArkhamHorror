module Arkham.Types.Location.Cards.BaseOfTheHill
  ( baseOfTheHill
  , BaseOfTheHill(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (baseOfTheHill)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Window

newtype BaseOfTheHill = BaseOfTheHill LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseOfTheHill :: LocationId -> BaseOfTheHill
baseOfTheHill = BaseOfTheHill . baseAttrs
  Cards.baseOfTheHill
  3
  (Static 0)
  Triangle
  [Square, Plus, Squiggle, Hourglass]

instance HasModifiersFor env BaseOfTheHill where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility
      (toSource attrs)
      1
      (ActionAbility (Just Action.Investigate) (ActionCost 1))
    & (abilityLimitL .~ PlayerLimit PerRound 1)

instance ActionRunner env => HasActions env BaseOfTheHill where
  getActions iid NonFast (BaseOfTheHill attrs) | iid `on` attrs =
    withBaseActions iid NonFast attrs
      $ pure
      $ [ ActivateCardAbilityAction iid (ability attrs)
        | locationRevealed attrs
        ]
      ++ [resignAction iid attrs]
  getActions iid window (BaseOfTheHill attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BaseOfTheHill where
  runMessage msg l@(BaseOfTheHill attrs) = case msg of
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
          (fmap (== "Diverging Path") . getName)
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
      isDivergingPath <- (== "Diverging Path") <$> getName lid
      if isDivergingPath
        then BaseOfTheHill
          <$> runMessage msg (attrs & connectedLocationsL %~ insertSet lid)
        else BaseOfTheHill <$> runMessage msg attrs
    _ -> BaseOfTheHill <$> runMessage msg attrs
