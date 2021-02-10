module Arkham.Types.Location.Cards.DowntownFirstBankOfArkham
  ( DowntownFirstBankOfArkham(..)
  , downtownFirstBankOfArkham
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Window
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownFirstBankOfArkham :: DowntownFirstBankOfArkham
downtownFirstBankOfArkham = DowntownFirstBankOfArkham $ baseAttrs
  "01130"
  (Name "Downtown" $ Just "First Bank of Arkham")
  EncounterSet.TheMidnightMasks
  3
  (PerPlayer 1)
  Triangle
  [Moon, T]
  [Arkham]

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
