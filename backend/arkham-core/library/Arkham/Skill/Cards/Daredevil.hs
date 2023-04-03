module Arkham.Skill.Cards.Daredevil
  ( daredevil
  , Daredevil(..)
  )
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.ClassSymbol
import Arkham.Deck
import Arkham.Message
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Daredevil = Daredevil SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daredevil :: SkillCard Daredevil
daredevil =
  skill Daredevil Cards.daredevil

instance RunMessage Daredevil where
  runMessage msg s@(Daredevil attrs) = case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      push $ DiscardUntilFirst iid (toSource attrs) $ CommittableCard iid $ BasicCardMatch $ CardWithClass Rogue <> CardWithType SkillType
      pure s
    RequestedPlayerCard iid (isSource attrs -> True) mcard discards -> do
      let weaknesses = filter (`cardMatch` WeaknessCard) discards
      pushAll $
        [CommitCard iid (PlayerCard c) | c <- maybeToList mcard]
        <> [ShuffleCardsIntoDeck (InvestigatorDeck iid) (map PlayerCard weaknesses) | notNull weaknesses]
      pure s
    _ -> Daredevil <$> runMessage msg attrs
