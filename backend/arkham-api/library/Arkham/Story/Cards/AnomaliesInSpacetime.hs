module Arkham.Story.Cards.AnomaliesInSpacetime (anomaliesInSpacetime) where

import Arkham.Ability
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, nextSkillTestModifier)
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name (toTitle)
import Arkham.Projection
import Arkham.Question (AmountTarget (..))
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AnomaliesInSpacetime = AnomaliesInSpacetime StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor AnomaliesInSpacetime where
  getModifiersFor (AnomaliesInSpacetime a) =
    modifySelect
      a
      Anyone
      [CannotTriggerAbilityMatching (AbilityOnLocation $ LocationWithHorror (atLeast 1))]

anomaliesInSpacetime :: StoryCard AnomaliesInSpacetime
anomaliesInSpacetime =
  storyWith AnomaliesInSpacetime Cards.anomaliesInSpacetime (flippedL .~ True) & persistStory

instance HasAbilities AnomaliesInSpacetime where
  getAbilities (AnomaliesInSpacetime a) =
    [ restricted a 1 (youExist $ InvestigatorAt (LocationWithHorror $ atLeast 1)) actionAbility
    , mkAbility a 2 $ forced $ RoundEnds #when
    ]

instance RunMessage AnomaliesInSpacetime where
  runMessage msg s@(AnomaliesInSpacetime attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- iid.clues
      sid <- getRandom
      chooseAmounts iid "Clues to spend" (MaxAmountTarget clues) [("Clues", (0, clues))] attrs
      chooseOneM iid $ withI18n do
        chooseTest #willpower 3 $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
        chooseTest #agility 3 $ beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure s
    ResolveAmounts iid (getChoiceAmount "Clues" -> n) (isTarget attrs -> True) -> do
      when (n > 0) do
        spendClues iid n
        nextSkillTestModifier iid (attrs.ability 1) iid (AnySkillValue (3 * n))
      pure s
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      getLocationOf iid >>= traverse_ \lid -> do
        let removeCount = if n >= 3 then 2 else 1
        horror <- field LocationHorror lid
        removeTokens (attrs.ability 1) lid #horror (min removeCount horror)
        founded <- remembered CorriganIndustriesHasBeenFounded
        when (n >= 4 && founded) do
          title <- field LocationName lid
          others <-
            select
              $ LocationWithTitle (toTitle title)
              <> not_ (LocationWithId lid)
              <> LocationWithHorror (atLeast 1)
          when (notNull others) do
            chooseOrRunOneM iid do
              targets others \other -> removeTokens (attrs.ability 1) other #horror 1
      doStep 1 msg
      pure s
    DoStep 1 (PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) _) -> do
      checkObjective attrs
      pure s
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      checkObjective attrs
      pure s
    _ -> AnomaliesInSpacetime <$> liftRunMessage msg attrs

checkObjective :: ReverseQueue m => StoryAttrs -> m ()
checkObjective attrs = do
  step <- getCurrentAgendaStep
  anyAnomalies <- selectAny $ LocationWithHorror (atLeast 1)
  when (step == 2 && not anyAnomalies) do
    lead <- getLead
    addToVictory lead attrs
