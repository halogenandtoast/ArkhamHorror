module Arkham.Story.Cards.AnomaliesInSpacetime (anomaliesInSpacetime) where

import Arkham.Ability
import Arkham.Agenda.Sequence
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getLead)
import Arkham.Investigator.Projection ()
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name (toTitle)
import Arkham.Projection
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
      [CannotTriggerAbilityMatching (AbilityOnLocation LocationWithAnyHorror <> not_ BasicAbility)]

anomaliesInSpacetime :: StoryCard AnomaliesInSpacetime
anomaliesInSpacetime =
  storyWith AnomaliesInSpacetime Cards.anomaliesInSpacetime (flippedL .~ True) & persistStory

instance HasAbilities AnomaliesInSpacetime where
  getAbilities (AnomaliesInSpacetime a) =
    [ restricted a 1 (youExist $ at_ LocationWithAnyHorror)
        $ actionAbilityWithCost
        $ OptionalCost ClueCostX
    , onlyOnce
        $ restricted a 2 (notExists LocationWithAnyHorror <> exists (AgendaWithSequence $ Sequence 2 A))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage AnomaliesInSpacetime where
  runMessage msg s@(AnomaliesInSpacetime attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePayment -> n) -> do
      sid <- getRandom
      when (n > 0) $ modifyAnySkill sid (attrs.ability 1) iid (3 * n)
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #agility] (Fixed 3)
      pure s
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      withLocationOf iid \lid -> do
        let removeCount = if n >= 3 then 2 else 1
        horror <- field LocationHorror lid
        removeTokens (attrs.ability 1) lid #horror (min removeCount horror)
        founded <- remembered CorriganIndustriesHasBeenFounded
        when (n >= 4 && founded) do
          title <- fieldMap LocationName toTitle lid
          others <- select $ LocationWithTitle title <> not_ (LocationWithId lid) <> LocationWithAnyHorror
          chooseOrRunOneM iid $ targets others $ removeTokensOn (attrs.ability 1) #horror 1
      pure s
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      lead <- getLead
      addToVictory lead attrs
      pure s
    _ -> AnomaliesInSpacetime <$> liftRunMessage msg attrs
