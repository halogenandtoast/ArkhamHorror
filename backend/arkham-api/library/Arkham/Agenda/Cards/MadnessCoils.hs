module Arkham.Agenda.Cards.MadnessCoils (MadnessCoils (..), madnessCoils) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
  getAbilities (MadnessCoils (a `With` _))
    | onSide A a =
        [ restricted a 1 (exists $ EnemyWithTitle "Hastur" <> EnemyWithDamage (AtLeast $ PerPlayer 3))
            $ Objective
            $ forced AnyWindow
        ]
  getAbilities _ = []

instance RunMessage MadnessCoils where
  runMessage msg a@(MadnessCoils (attrs `With` metadata)) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      let skills = setFromList [#willpower, #intellect] `difference` chosenSkills metadata
      lead <- getLead
      investigators <- getInvestigators
      sid <- getRandom
      chooseOneM lead do
        for_ (setToList skills) \sk -> do
          labeled ("Any investigator tests " <> format sk <> " (4)") do
            chooseOrRunOneM lead do
              targets investigators \iid -> beginSkillTest sid iid attrs attrs sk (Fixed 4)
        labeled
          "This can't be real. This can't be real. This can't be real. Each investigator takes 2 horror. Advance to agenda 2a."
          do
            for_ investigators \iid -> assignHorror iid attrs 2
            advanceAgendaDeck attrs
        labeled
          "The investigators faint and awaken some time later. Advance to agenda 2a and place 1 doom on it."
          do
            advanceAgendaDeck attrs
            placeDoomOnAgenda 1
      pure a
    FailedSkillTest _ _ source SkillTestInitiatorTarget {} (SkillSkillTest st) _ | isSource attrs source -> do
      afterSkillTest $ push $ AdvanceAgenda (toId attrs)
      pure $ MadnessCoils $ attrs `with` Metadata (insertSet st $ chosenSkills metadata)
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      afterSkillTest $ advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceAgenda (toId attrs)
      pure a
    _ -> MadnessCoils . (`with` metadata) <$> liftRunMessage msg attrs
