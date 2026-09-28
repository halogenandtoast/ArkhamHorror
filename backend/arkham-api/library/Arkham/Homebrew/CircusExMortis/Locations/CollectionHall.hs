module Arkham.Homebrew.CircusExMortis.Locations.CollectionHall (collectionHall) where

import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isParley)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype CollectionHall = CollectionHall LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collectionHall :: LocationCard CollectionHall
collectionHall = location CollectionHall Cards.collectionHall 5 (PerPlayer 1)

instance HasModifiersFor CollectionHall where
  getModifiersFor (CollectionHall a) = do
    viceShroudReduction a Opulence
    whenJustM getSkillTestInvestigator \iid -> maybeModified_ a iid do
      liftGuardM isParley
      liftGuardM $ iid <=~> investigatorAt a
      resources <- lift $ field InvestigatorResources iid
      guard $ resources >= 5
      pure [AnySkillValue $ resources `div` 5]
