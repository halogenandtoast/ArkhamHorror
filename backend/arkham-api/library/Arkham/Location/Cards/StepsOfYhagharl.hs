module Arkham.Location.Cards.StepsOfYhagharl (stepsOfYhagharl) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards (stepsOfYhagharl)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait

newtype StepsOfYhagharl = StepsOfYhagharl LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfYhagharl :: LocationCard StepsOfYhagharl
stepsOfYhagharl = location StepsOfYhagharl Cards.stepsOfYhagharl 3 (PerPlayer 1)

instance HasAbilities StepsOfYhagharl where
  getAbilities (StepsOfYhagharl attrs) =
    extendRevealed1 attrs $ skillTestAbility $ mkAbility attrs 1 $ forced $ Leaves #when You (be attrs)

instance RunMessage StepsOfYhagharl where
  runMessage msg l@(StepsOfYhagharl attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      encounterDiscard <- scenarioField ScenarioDiscard
      for_ (find (member Madness . toTraits) encounterDiscard) (drawCard iid)
      StepsOfYhagharl <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      cancelMovement (attrs.ability 1) iid
      shuffleBackIntoEncounterDeck attrs
      pure l
    _ -> StepsOfYhagharl <$> liftRunMessage msg attrs
