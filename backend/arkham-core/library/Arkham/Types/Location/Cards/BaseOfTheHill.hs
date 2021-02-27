module Arkham.Types.Location.Cards.BaseOfTheHill
  ( baseOfTheHill
  , BaseOfTheHill(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Trait
import Arkham.Types.Window

newtype BaseOfTheHill = BaseOfTheHill LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseOfTheHill :: BaseOfTheHill
baseOfTheHill = BaseOfTheHill $ baseAttrs
  "02282"
  (Name "Base of the Hill" Nothing)
  EncounterSet.WhereDoomAwaits
  3
  (Static 0)
  Triangle
  [Square, Plus, Squiggle, Hourglass]
  [Dunwich, SentinelHill]

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
  getActions iid NonFast (BaseOfTheHill attrs) =
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
        locations <- map unSetAsideLocationId <$> getSetList ()
        alteredPaths <- filterM
          (fmap (== mkName "Diverging Path") . getName)
          locations
        case nonEmpty alteredPaths of
          Just ne -> l <$ (unshiftMessage . PlaceLocation =<< sample ne)
          Nothing -> pure l
    AddConnection lid _ | toId attrs /= lid -> do
      isDivergingPath <- (== mkName "Diverging Path") <$> getName lid
      if isDivergingPath
        then BaseOfTheHill
          <$> runMessage msg (attrs & connectedLocationsL %~ insertSet lid)
        else BaseOfTheHill <$> runMessage msg attrs
    _ -> BaseOfTheHill <$> runMessage msg attrs
