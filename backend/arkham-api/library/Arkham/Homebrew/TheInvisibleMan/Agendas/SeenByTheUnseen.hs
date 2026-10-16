{- | Agenda 1, Seen by the Unseen.

The only agenda in the deck, and the engine of the whole scenario: it is what
turns every swing at Griffin into a draw from the Invisible Combat deck and
every dodge into a draw from the Invisible Escape deck.
-}
module Arkham.Homebrew.TheInvisibleMan.Agendas.SeenByTheUnseen (seenByTheUnseen) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher

newtype SeenByTheUnseen = SeenByTheUnseen AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seenByTheUnseen :: AgendaCard SeenByTheUnseen
seenByTheUnseen = agenda (1, A) SeenByTheUnseen Cards.seenByTheUnseen (Static 15)

{- | Two printed __Forced__ abilities, each with two triggers, split here into
four because the automatic halves must not re-trigger on what the initiation
halves already covered.

"You would automatically deal damage" is damage dealt without an attack, so the
combat half reads 'NonAttackDamageEffect' rather than excluding the attack by
its skill test -- the damage an attack deals lands outside the test the
initiation half fired on, so a criterion over 'DuringSkillTest' let it through
and drew a second card per attack.
-}
instance HasAbilities SeenByTheUnseen where
  getAbilities (SeenByTheUnseen a) =
    [ scenarioI18n
        $ withI18nTooltip "seenByTheUnseen.combat"
        $ mkAbility a 1
        $ forced
        $ InitiatedSkillTest #when You #any #any (WhileAttackingAnEnemy griffin)
    , scenarioI18n
        $ withI18nTooltip "seenByTheUnseen.combat"
        $ mkAbility a 2
        $ forced
        $ EnemyDealtDamage #when NonAttackDamageEffect griffin (SourceUsedBy You)
    , scenarioI18n
        $ withI18nTooltip "seenByTheUnseen.escape"
        $ mkAbility a 3
        $ forced
        $ InitiatedSkillTest #when You #any #any (WhileEvadingAnEnemy griffin)
    , scenarioI18n
        $ withI18nTooltip "seenByTheUnseen.escape"
        $ restricted a 4 (not_ $ DuringSkillTest $ WhileEvadingAnEnemy griffin)
        $ forced
        $ EnemyWouldBeEvaded #when You griffin
    ]

instance RunMessage SeenByTheUnseen where
  runMessage msg a@(SeenByTheUnseen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [1, 2] -> do
      drawInvisibleCombat iid
      pure a
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [3, 4] -> do
      drawInvisibleEscape iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    _ -> SeenByTheUnseen <$> liftRunMessage msg attrs
