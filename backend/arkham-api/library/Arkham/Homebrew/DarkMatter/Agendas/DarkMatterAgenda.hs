module Arkham.Homebrew.DarkMatter.Agendas.DarkMatterAgenda (darkMatterAgenda) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message.Lifted.Move

newtype DarkMatterAgenda = DarkMatterAgenda AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "(Keep adding doom to this agenda.)" — no doom threshold.
darkMatterAgenda :: AgendaCard DarkMatterAgenda
darkMatterAgenda =
  agendaWith (1, A) DarkMatterAgenda Cards.darkMatter (Static 0) $ doomThresholdL .~ Nothing

{- | "Forced - When a location has 3 or more horror on it: Remove it from the
game. Each investigator at that location is defeated. Move all enemies at that
location to the location with the most clues." /
"Forced - If each investigator has resigned or has been defeated, advance."
-}
instance HasAbilities DarkMatterAgenda where
  getAbilities (DarkMatterAgenda a) =
    [ restricted a 1 (exists $ LocationWithHorror $ atLeast 3) $ forced AnyWindow
    , restricted a 2 (not_ $ exists UneliminatedInvestigator) $ forced AnyWindow
    ]

instance RunMessage DarkMatterAgenda where
  runMessage msg a@(DarkMatterAgenda attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      doomed <- select $ LocationWithHorror $ atLeast 3
      destination <- selectOne $ LocationWithMostClues Anywhere
      for_ doomed \lid -> do
        for_ destination \dest -> do
          enemies <- select $ EnemyAt (LocationWithId lid)
          for_ enemies \eid -> enemyMoveTo (attrs.ability 1) eid dest
        eachInvestigator \iid ->
          whenM (iid <=~> InvestigatorAt (LocationWithId lid))
            $ push
            $ InvestigatorDefeated (toSource attrs) iid
        push $ RemoveLocation lid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgendaDeck attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> DarkMatterAgenda <$> liftRunMessage msg attrs
