module Arkham.Treachery.Cards.MorbidAwareness (
  morbidAwareness,
  MorbidAwareness (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Distance
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MorbidAwareness = MorbidAwareness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

morbidAwareness :: TreacheryCard MorbidAwareness
morbidAwareness = treachery MorbidAwareness Cards.morbidAwareness

instance RunMessage MorbidAwareness where
  runMessage msg t@(MorbidAwareness attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      l1 <- getJustLocation iid
      l2 <- getJustLocationByName "Room 212"
      mdistance <- getDistance l1 l2
      let n = 6 - maybe 0 unDistance mdistance
      push $ beginSkillTest iid (toSource attrs) iid #willpower n
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Place 1 of your clues on your location"
            [InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1]
          | hasClues
          ]
        <> [Label "Take 2 horror" [assignHorror iid (toSource attrs) 2]]

      pure t
    _ -> MorbidAwareness <$> runMessage msg attrs
