module Arkham.Types.Location.Cards.DowntownFirstBankOfArkham
  ( DowntownFirstBankOfArkham(..)
  , downtownFirstBankOfArkham
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (downtownFirstBankOfArkham)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownFirstBankOfArkham :: LocationCard DowntownFirstBankOfArkham
downtownFirstBankOfArkham = location
  DowntownFirstBankOfArkham
  Cards.downtownFirstBankOfArkham
  3
  (PerPlayer 1)
  Triangle
  [Moon, T]

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasAbilities env DowntownFirstBankOfArkham where
  getAbilities iid window@(Window Timing.When NonFast) (DowntownFirstBankOfArkham attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseAbilities iid window attrs $ do
      canGainResources <-
        notElem CannotGainResources
          <$> getInvestigatorModifiers iid (toSource attrs)
      pure [ locationAbility (ability attrs) | canGainResources ]
  getAbilities iid window (DowntownFirstBankOfArkham attrs) =
    getAbilities iid window attrs

instance (LocationRunner env) => RunMessage env DowntownFirstBankOfArkham where
  runMessage msg l@(DowntownFirstBankOfArkham attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (TakeResources iid 3 False)
    _ -> DowntownFirstBankOfArkham <$> runMessage msg attrs
