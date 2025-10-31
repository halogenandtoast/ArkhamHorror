module Arkham.Skill.Cards.Daredevil (daredevil) where

import Arkham.Card
import Arkham.Deck
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
      let weaknesses = filter (`cardMatch` WeaknessCard) discards
      pushAll
        $ [CommitCard iid (PlayerCard c) | c <- maybeToList mcard]
        <> [ ShuffleCardsIntoDeck (InvestigatorDeck iid) (map PlayerCard weaknesses)
           | notNull weaknesses && not (tabooed TabooList21 attrs)
           ]
      when (tabooed TabooList21 attrs && notNull weaknesses) do
        afterSkillTest iid "Daredevil" do
          ifCardExists (inDiscardOf iid <> mapOneOf CardWithId . map toCardId weaknesses) do
            chooseOneAtATimeM iid $ targets weaknesses $ addToHand iid . only
      pure s
    _ -> Daredevil <$> liftRunMessage msg attrs
