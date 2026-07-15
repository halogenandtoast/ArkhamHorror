module Arkham.Agenda.Cards.MadnessCoils (madnessCoils) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DimCarcosa.Helpers (scenarioI18n)
import Arkham.SkillTest.Type
import Arkham.SkillType

newtype Metadata = Metadata {chosenSkills :: Set SkillType}
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype MadnessCoils = MadnessCoils (AgendaAttrs `With` Metadata)
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessCoils :: AgendaCard MadnessCoils
madnessCoils = agenda (1, A) (MadnessCoils . (`with` Metadata mempty)) Cards.madnessCoils (Static 7)

instance HasAbilities MadnessCoils where
  getAbilities (MadnessCoils (a `With` _)) =
    guard (onSide A a)
      $> restricted
        a
        1
        (exists $ EnemyWithTitle "Hastur" <> EnemyWithDamage (AtLeast $ PerPlayer 3))
        (forced AnyWindow)

instance RunMessage MadnessCoils where
  runMessage msg a@(MadnessCoils (attrs `With` metadata)) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      let skills = setFromList [#willpower, #intellect] `difference` chosenSkills metadata
      lead <- getLead
      investigators <- getInvestigators
      sid <- getRandom
      scenarioI18n $ chooseOneM lead do
        for_ (setToList skills) \sk -> do
          withI18n $ countVar 4 $ skillVar sk $ labeled' "anyInvestigatorTests" do
            chooseOrRunOneM lead do
              targets investigators \iid -> beginSkillTest sid iid attrs attrs sk (Fixed 4)
        labeled' "madnessCoils.takeHorror" do
          for_ investigators \iid -> assignHorror iid attrs 2
          advanceAgendaDeck attrs
        labeled' "madnessCoils.faint" do
          advanceAgendaDeck attrs
          placeDoomOnAgenda 1
      pure a
    FailedSkillTest _ _ source SkillTestInitiatorTarget {} (SkillSkillTest st) _ | isSource attrs source -> do
      afterSkillTestQuiet $ advanceAgenda attrs
      pure $ MadnessCoils $ attrs `with` Metadata (insertSet st $ chosenSkills metadata)
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      afterSkillTestQuiet $ advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceAgenda attrs
      pure a
    _ -> MadnessCoils . (`with` metadata) <$> liftRunMessage msg attrs
