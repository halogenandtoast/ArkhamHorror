module Arkham.Event.Cards.MakingPreparations (makingPreparations, MakingPreparations (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier
import Arkham.Phase
import Arkham.SkillType

newtype Metadata = Metadata {chosenSkills :: [SkillType]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MakingPreparations = MakingPreparations (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

makingPreparations :: EventCard MakingPreparations
makingPreparations = event (MakingPreparations . (`with` Metadata [])) Cards.makingPreparations

instance RunMessage MakingPreparations where
  runMessage msg e@(MakingPreparations (With attrs meta)) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      let skills = [#willpower, #intellect, #combat, #agility]
      chooseN iid 2 [SkillLabel s [ForSkillType s msg] | (i, s) <- withIndex skills]
      doStep 1 msg
      pure e
    ForSkillType s (Revelation _iid (isSource attrs -> True)) -> do
      pure . MakingPreparations $ attrs `with` meta {chosenSkills = s : chosenSkills meta}
    DoStep _ (Revelation _iid (isSource attrs -> True)) -> do
      let skills = [#willpower, #intellect, #combat, #agility]
      let negativeSkills = filter (`notElem` chosenSkills meta) skills
      eachInvestigator \iid -> do
        for_ negativeSkills $ \s -> do
          endOfPhaseModifier InvestigationPhase attrs iid $ SkillModifier s (-1)
        for_ (chosenSkills meta) $ \s -> do
          endOfPhaseModifier InvestigationPhase attrs iid $ SkillModifier s 1
      pure e
    _ -> MakingPreparations . (`with` meta) <$> liftRunMessage msg attrs
