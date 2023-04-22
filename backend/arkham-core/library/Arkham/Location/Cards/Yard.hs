module Arkham.Location.Cards.Yard
  ( yard
  , Yard(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Source

newtype Yard = Yard LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yard :: LocationCard Yard
yard = location Yard Cards.yard 1 (PerPlayer 1)

instance HasModifiersFor Yard where
  getModifiersFor (LocationTarget lid) (Yard attrs) | lid == toId attrs = do
    mskillTestSource <- getSkillTestSource
    case mskillTestSource of
      Just (SkillTestSource iid _ source (Just Action.Investigate))
        | isSource attrs source -> do
          horror <- field InvestigatorHorror iid
          pure $ toModifiers
            attrs
            [ ShroudModifier horror | locationRevealed attrs ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities Yard where
  getAbilities (Yard attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
      $ ActionAbility Nothing
      $ Costs [ActionCost 1, DamageCost (toSource attrs) YouTarget 1]
    | locationRevealed attrs
    ]

instance RunMessage Yard where
  runMessage msg l@(Yard attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      l <$ push (Remember IncitedAFightAmongstThePatients)
    _ -> Yard <$> runMessage msg attrs
