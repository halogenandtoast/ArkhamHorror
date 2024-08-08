module Arkham.Skill.Cards.Daredevil (daredevil, Daredevil (..)) where

import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Deck
import Arkham.Matcher
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
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      push
        $ DiscardUntilFirst iid (toSource attrs) (InvestigatorDeck iid)
        $ CommittableCard (InvestigatorWithId iid)
        $ BasicCardMatch
        $ CardWithClass Rogue
        <> CardWithType SkillType
      Daredevil <$> liftRunMessage msg attrs
    RequestedPlayerCard iid (isSource attrs -> True) mcard discards -> do
      let weaknesses = filter (`cardMatch` WeaknessCard) discards
      pushAll
        $ [CommitCard iid (PlayerCard c) | c <- maybeToList mcard]
        <> [ ShuffleCardsIntoDeck (InvestigatorDeck iid) (map PlayerCard weaknesses)
           | notNull weaknesses && not (tabooed TabooList21 attrs)
           ]
      when (tabooed TabooList21 attrs && notNull weaknesses) do
        afterSkillTest do
          chooseOneAtATime
            iid
            [targetLabel weakness [AddToHand iid [toCard weakness]] | weakness <- weaknesses]

      pure s
    _ -> Daredevil <$> liftRunMessage msg attrs
