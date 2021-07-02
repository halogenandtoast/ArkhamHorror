module Arkham.Types.Location.Cards.DowntownFirstBankOfArkham
  ( DowntownFirstBankOfArkham(..)
  , downtownFirstBankOfArkham
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (downtownFirstBankOfArkham)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Window

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownFirstBankOfArkham :: LocationId -> DowntownFirstBankOfArkham
downtownFirstBankOfArkham = DowntownFirstBankOfArkham . baseAttrs
  Cards.downtownFirstBankOfArkham
  3
  (PerPlayer 1)
  Triangle
  [Moon, T]

instance HasModifiersFor env DowntownFirstBankOfArkham where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = PlayerLimit PerGame 1
    }

instance ActionRunner env => HasActions env DowntownFirstBankOfArkham where
  getActions iid NonFast (DowntownFirstBankOfArkham attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      canGainResources <-
        notElem CannotGainResources
        . map modifierType
        <$> getInvestigatorModifiers iid (toSource attrs)
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | canGainResources && iid `member` locationInvestigators
        ]
  getActions iid window (DowntownFirstBankOfArkham attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env DowntownFirstBankOfArkham where
  runMessage msg l@(DowntownFirstBankOfArkham attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (TakeResources iid 3 False)
    _ -> DowntownFirstBankOfArkham <$> runMessage msg attrs
