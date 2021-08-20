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
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

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
  getAbilities iid window@(Window Timing.When NonFast) (CursedShores attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseActions iid window attrs $ pure
      [ locationAbility
          (mkAbility attrs 1 $ ActionAbility Nothing $ ActionCost 1)
      ]
  getAbilities i window (CursedShores attrs) = getAbilities i window attrs

instance LocationRunner env => RunMessage env CursedShores where
  runMessage msg l@(CursedShores attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ pushAll
      [ InvestigatorAssignDamage iid source DamageAny 1 0
      , CreateEffect "81007" Nothing (toSource attrs) (InvestigatorTarget iid)
      ]
    WhenEnterLocation iid lid
      | -- TODO: SHOULD WE BROADCAST LRAVING THE LOCATION INSTEAD
        lid /= locationId && iid `elem` locationInvestigators -> do
        skillCards <- map unHandCardId <$> getSetList (iid, SkillType)
        case skillCards of
          [] -> pure ()
          [x] -> push (DiscardCard iid x)
          xs -> push (chooseOne iid [ DiscardCard iid x | x <- xs ])
        CursedShores <$> runMessage msg attrs
    _ -> CursedShores <$> runMessage msg attrs
