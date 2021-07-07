module Arkham.Types.Location.Cards.MiskatonicUniversity
  ( MiskatonicUniversity(..)
  , miskatonicUniversity
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (miskatonicUniversity)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype MiskatonicUniversity = MiskatonicUniversity LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversity :: LocationCard MiskatonicUniversity
miskatonicUniversity = location
  MiskatonicUniversity
  Cards.miskatonicUniversity
  4
  (PerPlayer 2)
  Diamond
  [T, Plus, Circle, Square]

instance HasModifiersFor env MiskatonicUniversity where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MiskatonicUniversity where
  getActions iid NonFast (MiskatonicUniversity attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ UseAbility
          iid
          (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
      | iid `member` locationInvestigators
      ]
  getActions iid window (MiskatonicUniversity attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (SearchTopOfDeck
        iid
        source
        (InvestigatorTarget iid)
        6
        [Tome, Spell]
        (ShuffleBackIn $ DrawFound iid)
      )
    _ -> MiskatonicUniversity <$> runMessage msg attrs
