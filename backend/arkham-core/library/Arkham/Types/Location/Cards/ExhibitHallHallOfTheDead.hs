module Arkham.Types.Location.Cards.ExhibitHallHallOfTheDead
  ( exhibitHallHallOfTheDead
  , ExhibitHallHallOfTheDead(..)
  )
where


import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ExhibitHallHallOfTheDead = ExhibitHallHallOfTheDead LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallHallOfTheDead :: ExhibitHallHallOfTheDead
exhibitHallHallOfTheDead = ExhibitHallHallOfTheDead
  $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "02136"
    (Name "Exhibit Hall" $ Just "Hall of the Dead")
    EncounterSet.TheMiskatonicMuseum
    3
    (PerPlayer 2)
    Squiggle
    [Square, Hourglass]
    [Miskatonic, Exhibit]

instance HasModifiersFor env ExhibitHallHallOfTheDead where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ExhibitHallHallOfTheDead where
  getActions iid window (ExhibitHallHallOfTheDead attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallHallOfTheDead where
  runMessage msg l@(ExhibitHallHallOfTheDead attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l <$ unshiftMessage
        (InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1)
    _ -> ExhibitHallHallOfTheDead <$> runMessage msg attrs
