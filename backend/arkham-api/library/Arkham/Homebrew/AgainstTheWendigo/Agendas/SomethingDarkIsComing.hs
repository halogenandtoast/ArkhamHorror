module Arkham.Homebrew.AgainstTheWendigo.Agendas.SomethingDarkIsComing (
  somethingDarkIsComing,
) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher

newtype SomethingDarkIsComing = SomethingDarkIsComing AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingDarkIsComing :: AgendaCard SomethingDarkIsComing
somethingDarkIsComing = agenda (2, A) SomethingDarkIsComing Cards.somethingDarkIsComing (Static 6)

instance RunMessage SomethingDarkIsComing where
  runMessage msg a@(SomethingDarkIsComing attrs) = runQueueT $ case msg of
    {- | Agenda 2's @b@ side is the Bestial Creature, so advancing it takes the
    agenda out of the deck and puts the enemy into play rather than continuing
    the story; agenda 3 is underneath. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      createEnemyAtLocationMatching_ Enemies.bestialCreature (LocationWithMostInvestigators Anywhere)
      advanceAgendaDeck attrs
      pure a
    _ -> SomethingDarkIsComing <$> liftRunMessage msg attrs
