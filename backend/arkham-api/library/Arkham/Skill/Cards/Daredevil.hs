module Arkham.Skill.Cards.Daredevil (daredevil) where

import Arkham.Card
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Taboo

newtype Daredevil = Daredevil SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daredevil :: SkillCard Daredevil
daredevil = skill Daredevil Cards.daredevil

instance RunMessage Daredevil where
  runMessage msg s@(Daredevil attrs) = runQueueT $ case msg of
    Do (CommitCard iid card) | attrs.cardId == card.id -> do
      chooseThisM iid attrs do
        discardUntilFirst iid attrs iid
          $ CommittableCard (InvestigatorWithId iid)
          $ basic (#rogue <> #skill)
      Daredevil <$> liftRunMessage msg attrs
    RequestedPlayerCard iid (isSource attrs -> True) mcard discards -> do
      focusCards (maybeToList mcard <> discards) do
        chooseOneM iid do
          case mcard of
            Nothing -> withI18n $ labeled' "continue" nothing
            Just c -> targeting c $ commitCard iid c

      let weaknesses = filter (`cardMatch` WeaknessCard) discards
      unless (attrs.tabooed TabooList21) $ shuffleCardsIntoDeck iid weaknesses

      when (attrs.tabooed TabooList21 && notNull weaknesses) do
        afterSkillTest iid "Daredevil" do
          ifCardExists (inDiscardOf iid <> basic (mapOneOf (CardWithId . toCardId) weaknesses)) do
            chooseOneAtATimeM iid $ targets weaknesses $ drawCard iid
      pure s
    _ -> Daredevil <$> liftRunMessage msg attrs
