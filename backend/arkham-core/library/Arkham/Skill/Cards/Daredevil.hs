module Arkham.Skill.Cards.Daredevil (
  daredevil,
  Daredevil (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Deck
import Arkham.Matcher
import Arkham.Message
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
      push
        $ DiscardUntilFirst iid (toSource attrs) (InvestigatorDeck iid)
        $ CommittableCard (InvestigatorWithId iid)
        $ BasicCardMatch
        $ CardWithClass Rogue
        <> CardWithType SkillType
      Daredevil <$> runMessage msg attrs
    RequestedPlayerCard iid (isSource attrs -> True) mcard discards -> do
      let weaknesses = filter (`cardMatch` WeaknessCard) discards
      pushAll
        $ [CommitCard iid (PlayerCard c) | c <- maybeToList mcard]
        <> [ ShuffleCardsIntoDeck (InvestigatorDeck iid) (map PlayerCard weaknesses)
           | notNull weaknesses
           ]
      pure s
    _ -> Daredevil <$> runMessage msg attrs
