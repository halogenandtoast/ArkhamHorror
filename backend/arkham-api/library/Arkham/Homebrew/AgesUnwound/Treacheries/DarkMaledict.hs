module Arkham.Homebrew.AgesUnwound.Treacheries.DarkMaledict (darkMaledict) where

import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers (unstuckI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype DarkMaledict = DarkMaledict TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkMaledict :: TreacheryCard DarkMaledict
darkMaledict = treachery DarkMaledict Cards.darkMaledict

{- | "Peril. Revelation - For each doom in play (min 1), choose a different
option:
-- Choose an investigator to take 2 horror.
-- Choose a damaged enemy and heal 2 damage from it.
-- Each investigator chooses and discards a card from their hand."

"A different option" each time, so this is one @chooseN@ over the three printed
options rather than a per-doom loop -- the options are distinct by construction
and there are only three of them, so more than three doom changes nothing.
Peril means the drawing investigator resolves it alone.
-}
instance RunMessage DarkMaledict where
  runMessage msg t@(DarkMaledict attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      doom <- getDoomCount
      investigators <- select UneliminatedInvestigator
      damagedEnemies <- select $ EnemyWithDamage (atLeast 1)
      {- An option with nothing to choose is left out rather than offered
      disabled: a `chooseN` asked for more picks than it has selectable choices
      cannot be answered. The count is clamped to what is on offer for the same
      reason -- and "for each doom in play" never needs a fourth option, since
      the card prints only three. -}
      let offered = if notNull damagedEnemies then 3 else 2
      chooseNM iid (min offered (max 1 doom)) $ unstuckI18n $ scope "darkMaledict" do
        labeled "investigatorTakesHorror"
          $ chooseOrRunOneM iid
          $ targets investigators \i -> assignHorror i attrs 2
        when (notNull damagedEnemies)
          $ labeled "healDamagedEnemy"
          $ chooseOrRunOneM iid
          $ targets damagedEnemies \e -> healDamage e attrs 2
        labeled "eachInvestigatorDiscards"
          $ for_ investigators \i -> chooseAndDiscardCard i attrs
      pure t
    _ -> DarkMaledict <$> liftRunMessage msg attrs
