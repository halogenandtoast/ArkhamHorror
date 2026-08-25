module Arkham.Homebrew.DarkMatter.Agendas.DarkMatterAgenda (darkMatterAgenda) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Location.Types (Field (LocationCard))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype DarkMatterAgenda = DarkMatterAgenda AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Set aside at setup; Ritual of the Sun replaces the agenda deck with it as
agenda 1c, which is why Tassilda's Awakening advances "to agenda 1d".

"(Keep adding doom to this agenda.)" — no doom threshold.
-}
darkMatterAgenda :: AgendaCard DarkMatterAgenda
darkMatterAgenda =
  agendaWith (1, C) DarkMatterAgenda Cards.darkMatter (Static 0) $ doomThresholdL .~ Nothing

{- | "Forced - When a location has 3 or more horror on it: Remove it from the
game. Each investigator at that location is defeated. Move all enemies at that
location to the location with the most clues."
-}
instance HasAbilities DarkMatterAgenda where
  getAbilities (DarkMatterAgenda a)
    | onSide C a =
        [ restricted a 1 (exists $ LocationWithHorror (atLeast 3) <> not_ LocationBeingRemoved)
            $ forced AnyWindow
        ]
    | otherwise = []

instance RunMessage DarkMatterAgenda where
  runMessage msg a@(DarkMatterAgenda attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      doomed <- select $ LocationWithHorror (atLeast 3) <> not_ LocationBeingRemoved
      destination <- selectOne $ LocationWithMostClues Anywhere
      for_ doomed \lid -> do
        for_ destination \dest ->
          selectEach (EnemyAt (LocationWithId lid)) \eid -> enemyMoveTo (attrs.ability 1) eid dest
        selectEach (InvestigatorAt (LocationWithId lid)) $ investigatorDefeated attrs
        -- Tassilda's Awakening reads back which colonies were removed, so the
        -- card has to be recorded as removed from the game, not just delisted.
        card <- field LocationCard lid
        push $ RemoveLocation lid
        push $ RemovedFromGame card
      pure a
    {- "Forced - If each investigator has resigned or has been defeated,
    advance." Claimed as the scenario's no-remaining-investigators handler by
    Ritual of the Sun when this agenda comes into play: that hook is the only
    one that fires for every elimination path (the last investigator's own
    windows are skipped once nobody is left). -}
    HandleNoRemainingInvestigators (isTarget attrs -> True) -> do
      if onSide D attrs then gameOver else advanceAgenda attrs
      pure a
    {- Agenda 1d:

    "Each investigator is killed. The investigators lose the campaign. There is
    no resolution." -}
    AdvanceAgenda (isSide D attrs -> True) -> do
      push $ SetNoRemainingInvestigatorsHandler (toTarget attrs)
      eachInvestigator $ kill attrs
      gameOver
      pure a
    _ -> DarkMatterAgenda <$> liftRunMessage msg attrs
