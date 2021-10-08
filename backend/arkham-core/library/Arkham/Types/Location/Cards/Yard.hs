module Arkham.Types.Location.Cards.Yard
  ( yard
  , Yard(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillTest
import Arkham.Types.Source
import Arkham.Types.Target

newtype Yard = Yard LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yard :: LocationCard Yard
yard = location Yard Cards.yard 1 (PerPlayer 1) Diamond [Circle, Plus]

instance
  ( HasCount HorrorCount env InvestigatorId
  , HasSkillTest env
  )
  => HasModifiersFor env Yard where
  getModifiersFor _ (LocationTarget lid) (Yard attrs) | lid == toId attrs = do
    mskillTestSource <- getSkillTestSource
    case mskillTestSource of
      Just (SkillTestSource iid _ source _ (Just Action.Investigate))
        | isSource attrs source -> do
          horror <- unHorrorCount <$> getCount iid
          pure $ toModifiers
            attrs
            [ ShroudModifier horror | locationRevealed attrs ]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities Yard where
  getAbilities (Yard attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
      $ ActionAbility Nothing
      $ Costs [ActionCost 1, DamageCost (toSource attrs) YouTarget 1]
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env Yard where
  runMessage msg l@(Yard attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (Remember IncitedAFightAmongstThePatients)
    _ -> Yard <$> runMessage msg attrs
