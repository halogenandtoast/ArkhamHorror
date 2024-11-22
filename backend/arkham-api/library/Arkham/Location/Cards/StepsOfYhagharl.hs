module Arkham.Location.Cards.StepsOfYhagharl (stepsOfYhagharl, StepsOfYhagharl (..)) where

import Arkham.Ability
import Arkham.Classes.HasQueue (replaceMessageMatching)
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (stepsOfYhagharl)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Movement
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait

newtype StepsOfYhagharl = StepsOfYhagharl LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfYhagharl :: LocationCard StepsOfYhagharl
stepsOfYhagharl =
  location StepsOfYhagharl Cards.stepsOfYhagharl 3 (PerPlayer 1)

instance HasAbilities StepsOfYhagharl where
  getAbilities (StepsOfYhagharl attrs) =
    extendRevealed1 attrs $ skillTestAbility $ mkAbility attrs 1 $ forced $ Leaves #when You (be attrs)

instance RunMessage StepsOfYhagharl where
  runMessage msg l@(StepsOfYhagharl attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      encounterDiscard <- scenarioField ScenarioDiscard
      for_ (find (member Madness . toTraits) encounterDiscard) \madnessCard ->
        push $ InvestigatorDrewEncounterCard iid madnessCard
      StepsOfYhagharl <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      replaceMessageMatching
        ( \case
            MoveFrom _ iid' lid' -> iid == iid' && lid' == toId attrs
            _ -> False
        )
        (const [])
      replaceMessageMatching
        ( \case
            Will (MoveTo movement) | moveTarget movement == toTarget iid -> True
            Will (Move movement) | moveTarget movement == toTarget iid -> True
            _ -> False
        )
        (const [])
      replaceMessageMatching
        ( \case
            After (Move movement) | moveTarget movement == toTarget iid -> True
            _ -> False
        )
        (const [])
      replaceMessageMatching
        ( \case
            MoveTo movement | moveTarget movement == toTarget iid -> True
            _ -> False
        )
        (const [ShuffleBackIntoEncounterDeck $ toTarget attrs])
      pure l
    _ -> StepsOfYhagharl <$> liftRunMessage msg attrs
