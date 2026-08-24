module Arkham.Homebrew.DarkMatter.Acts.FirstEncounter (firstEncounter) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Card (genCard)
import Arkham.Helpers.Agenda (getDoomOnAgenda)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (getImpendingDoom, getMemories)
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))

newtype FirstEncounter = FirstEncounter ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstEncounter :: ActCard FirstEncounter
firstEncounter = act (1, A) FirstEncounter Cards.firstEncounter Nothing

-- "You cannot leave the Entrance Tunnel."
instance HasModifiersFor FirstEncounter where
  getModifiersFor (FirstEncounter a) =
    modifySelect a (InvestigatorAt $ locationIs Locations.entranceTunnel) [CannotMove]

{- | "Objective - If there are 1[per_investigator] clues on this act, or after The
Greys is defeated, advance."

The Greys' own defeat is always cancelled (it is flipped to the Mi-Go Scientist
in 1b instead of ever leaving play), so this watches the moment it WOULD be
defeated rather than 'EnemyDefeated', which would never fire.
-}
instance HasAbilities FirstEncounter where
  getAbilities (FirstEncounter a) =
    [ restricted a 1 (CluesOnThis $ AtLeast $ PerPlayer 1)
        $ Objective
        $ forced AnyWindow
    , mkAbility a 2
        $ Objective
        $ forced
        $ EnemyWouldBeDefeated #when (enemyIsExact Enemies.theGreys)
    ]

instance RunMessage FirstEncounter where
  runMessage msg a@(FirstEncounter attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) n | n `elem` [1, 2] -> do
      advanceVia #other attrs attrs
      pure a
    {- Act 1b:

    "Flip The Greys to its other side (transfer any damage tokens on it to its
    other side).
    Each investigator with 3 or fewer "Memories" takes 1 horror.
    Advance to agenda 2a and act 2a. Do not remove doom from play. Move all doom
    from agenda 1 to agenda 2a. Add 2 doom to agenda 2a for each tally mark
    under "Impending Doom" in your Campaign Log."

    The Greys' other side is the Mi-Go Scientist enemy (@:dark-matter:163b@);
    'Swap' carries the tokens (damage included) over. The Greys' own defeat is
    always cancelled, so it is guaranteed to still be in play here; flipping it
    can transfer 3+ damage onto the Mi-Go Scientist's lower health, so it must
    be checked for defeat immediately after.

    'do_' on 'AdvanceToAgenda' is what keeps the doom in play: the plain
    message is handled by the agenda runner, which prefixes a
    'RemoveAllDoomFromPlay'. -}
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      greys <- selectJust (enemyIsExact Enemies.theGreys)
      miGoScientist <- genCard Enemies.miGoScientist
      push $ ReplaceEnemy greys miGoScientist Swap
      checkDefeated attrs greys
      eachInvestigator \iid -> do
        memories <- getMemories iid
        when (memories <= 3) $ assignHorror iid attrs 1
      carriedOver <- getDoomOnAgenda
      impendingDoom <- getImpendingDoom
      do_ $ AdvanceToAgenda 1 Agendas.signsFromAldebaran Agenda.A (toSource attrs)
      placeDoomOnAgenda (carriedOver + 2 * impendingDoom)
      advanceActDeck attrs
      pure a
    _ -> FirstEncounter <$> liftRunMessage msg attrs
