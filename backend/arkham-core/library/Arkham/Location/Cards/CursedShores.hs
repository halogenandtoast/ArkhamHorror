module Arkham.Location.Cards.CursedShores
  ( CursedShores(..)
  , cursedShores
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (cursedShores)
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Investigator.Types (Field(..))
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype CursedShores = CursedShores LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedShores :: LocationCard CursedShores
cursedShores = location
  CursedShores
  Cards.cursedShores
  1
  (Static 0)
  Square
  [Plus, Triangle, Diamond, Hourglass]

instance HasAbilities CursedShores where
  getAbilities (CursedShores attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 1
        , mkAbility attrs 2
        $ ForcedAbility
        $ Leaves Timing.When You
        $ LocationWithId
        $ toId attrs
        ]
      else []

instance RunMessage CursedShores where
  runMessage msg l@(CursedShores attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ pushAll
      [ InvestigatorAssignDamage iid source DamageAny 1 0
      , CreateEffect "81007" Nothing (toSource attrs) (InvestigatorTarget iid)
      ]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      skillCards <- fieldMap InvestigatorHand (map toCardId . filter (`cardMatch` CardWithType SkillType)) iid
      l <$ case skillCards of
        [] -> pure ()
        [x] -> push (DiscardCard iid x)
        xs -> push (chooseOne iid [ DiscardCard iid x | x <- xs ])
    _ -> CursedShores <$> runMessage msg attrs
