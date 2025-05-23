module Arkham.Location.Cards.BrokenSteps_289 (brokenSteps_289) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Investigator (getCanLoseActions)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.BlackStarsRise.Helpers
import Arkham.Trait

newtype BrokenSteps_289 = BrokenSteps_289 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenSteps_289 :: LocationCard BrokenSteps_289
brokenSteps_289 = location BrokenSteps_289 Cards.brokenSteps_289 4 (Static 0)

instance HasAbilities BrokenSteps_289 where
  getAbilities (BrokenSteps_289 a) = extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage BrokenSteps_289 where
  runMessage msg l@(BrokenSteps_289 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canLoseActions <- getCanLoseActions iid
      omens <- filterCards (CardWithTrait Omen <> #treachery) <$> scenarioField ScenarioDiscard
      chooseOneM iid do
        when canLoseActions do
          withI18n $ countVar 1 $ labeled' "loseActions" $ loseActions iid (attrs.ability 1) 1
        for_ (take 1 omens) \c ->
          scenarioI18n $ labeled' "brokenSteps.omen" $ findAndDrawEncounterCard iid (CardWithId c.id)
      pure l
    _ -> BrokenSteps_289 <$> liftRunMessage msg attrs
