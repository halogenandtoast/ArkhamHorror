module Arkham.Types.Location.Cards.MiskatonicUniversity
  ( MiskatonicUniversity(..)
  , miskatonicUniversity
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MiskatonicUniversity = MiskatonicUniversity Attrs
  deriving newtype (Show, ToJSON, FromJSON)

miskatonicUniversity :: MiskatonicUniversity
miskatonicUniversity = MiskatonicUniversity $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "01129"
    (Name "Miskatonic University" Nothing)
    EncounterSet.TheMidnightMasks
    4
    (PerPlayer 2)
    Diamond
    [T, Plus, Circle, Square]
    [Arkham]

instance HasModifiersFor env MiskatonicUniversity where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MiskatonicUniversity where
  getActions iid NonFast (MiskatonicUniversity attrs@Attrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
      | iid `member` locationInvestigators
      ]
  getActions iid window (MiskatonicUniversity attrs) =
    getActions iid window attrs

instance (LocationRunner env) => RunMessage env MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> l <$ unshiftMessage
      (SearchTopOfDeck
        iid
        (InvestigatorTarget iid)
        6
        [Tome, Spell]
        ShuffleBackIn
      )
    _ -> MiskatonicUniversity <$> runMessage msg attrs
