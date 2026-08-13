module Arkham.Homebrew.DarkMatter.Agendas.Supernova (supernova) where

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
newtype Supernova = Supernova AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

supernova :: AgendaCard Supernova
supernova = agenda (3, A) Supernova Cards.supernova (Static 6)

instance HasAbilities Supernova where
  getAbilities (Supernova a) =
    [ restricted
        a
        1
        (DuringSkillTest AnySkillTest <> youExist (investigatorWithRecordCount Memories (atLeast 1)))
        $ FastAbility Free
    ]

instance RunMessage Supernova where
  runMessage msg a@(Supernova attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      crossOffMemories iid 1
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-1))
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> Supernova <$> liftRunMessage msg attrs
