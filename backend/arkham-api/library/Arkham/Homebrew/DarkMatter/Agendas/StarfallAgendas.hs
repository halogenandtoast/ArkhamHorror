module Arkham.Homebrew.DarkMatter.Agendas.StarfallAgendas (
  journeyAcrossSpace,
  redSun,
  supernova,
) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (crossOffMemories)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Matcher

{- | All three Starfall agendas print:

"[free] During a skill test, cross out 1 tally mark next to your 'Memories':
Reduce the difficulty of this test by 1."
-}
newtype StarfallAgenda = StarfallAgenda AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

journeyAcrossSpace :: AgendaCard StarfallAgenda
journeyAcrossSpace = agenda (1, A) StarfallAgenda Cards.journeyAcrossSpace (Static 12)

redSun :: AgendaCard StarfallAgenda
redSun = agenda (2, A) StarfallAgenda Cards.redSun (Static 5)

supernova :: AgendaCard StarfallAgenda
supernova = agenda (3, A) StarfallAgenda Cards.supernova (Static 6)

instance HasAbilities StarfallAgenda where
  getAbilities (StarfallAgenda a) =
    [ restricted
        a
        1
        (DuringSkillTest AnySkillTest <> youExist (investigatorWithRecordCount Memories (atLeast 1)))
        $ FastAbility Free
    ]

instance RunMessage StarfallAgenda where
  runMessage msg a@(StarfallAgenda attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      crossOffMemories iid 1
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-1))
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> StarfallAgenda <$> liftRunMessage msg attrs
