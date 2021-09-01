module Arkham.Types.Location.Cards.CursedShores
  ( CursedShores(..)
  , cursedShores
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (cursedShores)
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype CursedShores = CursedShores LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedShores :: LocationCard CursedShores
cursedShores = location
  CursedShores
  Cards.cursedShores
  1
  (Static 0)
  Square
  [Plus, Triangle, Diamond, Hourglass]

instance HasAbilities env CursedShores where
  getAbilities iid window (CursedShores attrs) =
    withBaseAbilities iid window attrs $ pure $ if locationRevealed attrs
      then
        [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 1
        , mkAbility attrs 2
        $ ForcedAbility
        $ Leaves Timing.When You
        $ LocationWithId
        $ toId attrs
        ]
      else []

instance LocationRunner env => RunMessage env CursedShores where
  runMessage msg l@(CursedShores attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ pushAll
      [ InvestigatorAssignDamage iid source DamageAny 1 0
      , CreateEffect "81007" Nothing (toSource attrs) (InvestigatorTarget iid)
      ]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      skillCards <- map unHandCardId <$> getSetList (iid, SkillType)
      l <$ case skillCards of
        [] -> pure ()
        [x] -> push (DiscardCard iid x)
        xs -> push (chooseOne iid [ DiscardCard iid x | x <- xs ])
    _ -> CursedShores <$> runMessage msg attrs
