module Arkham.Homebrew.DarkMatter.Agendas.JourneyAcrossSpace (journeyAcrossSpace) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (crossOffMemories)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Matcher

{- | Like every Starfall agenda, this prints:

"[free] During a skill test, cross out 1 tally mark next to your 'Memories':
Reduce the difficulty of this test by 1."
-}
newtype JourneyAcrossSpace = JourneyAcrossSpace AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

journeyAcrossSpace :: AgendaCard JourneyAcrossSpace
journeyAcrossSpace = agenda (1, A) JourneyAcrossSpace Cards.journeyAcrossSpace (Static 12)

instance HasAbilities JourneyAcrossSpace where
  getAbilities (JourneyAcrossSpace a) =
    [ restricted
        a
        1
        (DuringSkillTest AnySkillTest <> youExist (investigatorWithRecordCount Memories (atLeast 1)))
        $ FastAbility Free
    ]

instance RunMessage JourneyAcrossSpace where
  runMessage msg a@(JourneyAcrossSpace attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      crossOffMemories iid 1
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-1))
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> JourneyAcrossSpace <$> liftRunMessage msg attrs
