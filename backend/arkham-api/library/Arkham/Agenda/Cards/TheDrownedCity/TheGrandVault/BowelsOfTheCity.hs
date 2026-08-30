module Arkham.Agenda.Cards.TheDrownedCity.TheGrandVault.BowelsOfTheCity (bowelsOfTheCity) where

import Arkham.Agenda.CardDefs.TheDrownedCity.TheGrandVault qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (struggleForAir, strugglesForAir)
import Arkham.Deck qualified as Deck
import Arkham.Enemy.CardDefs.TheDrownedCity.TheInescapable qualified as Enemies
import Arkham.Helpers.Enemy (disengageEnemyFromAll)
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (StarSpawn))
import Arkham.Treachery.CardDefs.TheDrownedCity.TheInescapable qualified as Treacheries

newtype BowelsOfTheCity = BowelsOfTheCity AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bowelsOfTheCity :: AgendaCard BowelsOfTheCity
bowelsOfTheCity = agenda (1, A) BowelsOfTheCity Cards.bowelsOfTheCity (Static 3)

instance HasAbilities BowelsOfTheCity where
  getAbilities (BowelsOfTheCity a) = [strugglesForAir a 1]

instance RunMessage BowelsOfTheCity where
  runMessage msg a@(BowelsOfTheCity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      lid <- getJustLocation lead
      createSetAsideEnemy_ Enemies.theInescapable lid
      investigators <- select $ InvestigatorAt $ LocationWithId lid
      sid <- getRandom
      leadChooseOrRunOneM
        $ targets investigators \iid -> beginSkillTest sid iid attrs attrs #agility (Fixed 4)

      stillBehindYou <- getSetAsideCardsMatching $ cardIs Treacheries.stillBehindYou
      -- The Inescapable is a Star Spawn and its creation above is only queued, so it is
      -- still set aside here and must not be shuffled in as the random Star Spawn.
      starSpawns <-
        getSetAsideCardsMatching
          $ CardWithTrait StarSpawn
          <> not_ (cardIs Enemies.theInescapable)
      randomStarSpawn <- maybe (pure []) (fmap pure . sample) (nonEmpty starSpawns)
      shuffleCardsIntoDeck Deck.EncounterDeck (stillBehindYou <> randomStarSpawn)
      shuffleEncounterDiscardBackIn

      advanceAgendaDeck attrs
      pure a
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      whenJustM (selectOne $ enemyIs Enemies.theInescapable) \inescapable -> do
        disengageEnemyFromAll inescapable
        exhaustThis inescapable
      pure a
    _ -> BowelsOfTheCity <$> liftRunMessage msg attrs
