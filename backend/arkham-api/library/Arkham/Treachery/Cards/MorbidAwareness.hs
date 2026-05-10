module Arkham.Treachery.Cards.MorbidAwareness (morbidAwareness, MorbidAwareness (..)) where

import Arkham.Classes
import Arkham.Helpers.Investigator (canPlaceCluesOnYourLocation)
import Arkham.I18n
import Arkham.Message
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MorbidAwareness = MorbidAwareness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

morbidAwareness :: TreacheryCard MorbidAwareness
morbidAwareness = treachery MorbidAwareness Cards.morbidAwareness

instance RunMessage MorbidAwareness where
  runMessage msg t@(MorbidAwareness attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push
        $ revelationSkillTest sid iid attrs #willpower
        $ SubtractCalculation (Fixed 6) (DistanceFromCalculation iid "Room 212")
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      canPlaceClues <- canPlaceCluesOnYourLocation iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            (withI18n $ numberVar "count" 1 $ "$" <> ikey "label.placeCluesOnYourLocation")
            [InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1]
          | canPlaceClues
          ]
        <> [Label (withI18n $ numberVar "count" 2 $ "$" <> ikey "label.takeHorror") [assignHorror iid (toSource attrs) 2]]

      pure t
    _ -> MorbidAwareness <$> runMessage msg attrs
