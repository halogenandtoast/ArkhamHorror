module Arkham.Location.Cards.AbandonedCamp (abandonedCamp) where

import Arkham.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype AbandonedCamp = AbandonedCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedCamp :: LocationCard AbandonedCamp
abandonedCamp = location AbandonedCamp Cards.abandonedCamp 2 (PerPlayer 1)

instance HasAbilities AbandonedCamp where
  getAbilities (AbandonedCamp a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (youExist $ InvestigatorWithActionsPerformed (atLeast 1) <> InvestigatorWithAnyResources)
      $ forced
      $ RevealLocation #after You (be a)

instance RunMessage AbandonedCamp where
  runMessage msg l@(AbandonedCamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- fieldMap InvestigatorActionsPerformed length iid
      loseResources iid (attrs.ability 1) (n * 2)
      pure l
    _ -> AbandonedCamp <$> liftRunMessage msg attrs
