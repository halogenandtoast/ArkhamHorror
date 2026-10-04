module Arkham.Homebrew.TheSymphonyOfErichZann.Agendas.Crescendo (crescendo) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (scenarioI18n)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Musician)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Crescendo = Crescendo AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crescendo :: AgendaCard Crescendo
crescendo = agenda (2, A) Crescendo Cards.crescendo (Static 5)

instance RunMessage Crescendo where
  runMessage msg a@(Crescendo attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn

      -- "Each investigator with a [[Musician]] enemy at their location must test
      -- [agility] (3) or [intellect] (3)."
      threatened <- select $ InvestigatorAt $ LocationWithEnemy (EnemyWithTrait Musician)
      for_ threatened \iid -> do
        sid <- getRandom
        chooseOneM iid $ scenarioI18n $ scope "crescendo" do
          labeled "testAgility" $ beginSkillTest sid iid attrs iid #agility (Fixed 3)
          labeled "testIntellect" $ beginSkillTest sid iid attrs iid #intellect (Fixed 3)

      -- "Spawn the set-aside Young Nightingale enemy at the Gallery."
      gallery <- selectJust $ locationIs Locations.gallery
      nightingale <- getSetAsideCard Enemies.youngNightingale
      createEnemyAt_ nightingale gallery

      advanceAgendaDeck attrs
      pure a
    -- "If you fail, each [[Musician]] enemy at your location immediately attacks you."
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      musicians <- select $ EnemyWithTrait Musician <> enemyAtLocationWith iid
      for_ musicians \eid -> initiateEnemyAttack eid attrs iid
      pure a
    _ -> Crescendo <$> liftRunMessage msg attrs
