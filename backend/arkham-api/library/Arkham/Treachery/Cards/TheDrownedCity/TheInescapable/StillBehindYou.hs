module Arkham.Treachery.Cards.TheDrownedCity.TheInescapable.StillBehindYou (stillBehindYou) where

import Arkham.Enemy.CardDefs.TheDrownedCity.TheInescapable qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Treachery.CardDefs.TheDrownedCity.TheInescapable qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Zone

newtype StillBehindYou = StillBehindYou TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stillBehindYou :: TreacheryCard StillBehindYou
stillBehindYou = treachery StillBehindYou Cards.stillBehindYou

instance RunMessage StillBehindYou where
  runMessage msg t@(StillBehindYou attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      copies <- selectCount $ VictoryDisplayCardMatch $ basic $ cardIs Cards.stillBehindYou
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed $ 2 + copies)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      selectOne (enemyIs Enemies.theInescapable) >>= \case
        Just enemy -> withLocationOf iid \lid -> do
          enemyMoveToEdit attrs enemy lid \m -> m {moveMeans = OneAtATime}
          forTarget enemy msg
        Nothing -> do
          getSetAsideCardMaybe Enemies.theInescapable >>= \case
            Just card -> drawCard iid card
            Nothing ->
              findEncounterCardIn
                iid
                attrs
                (cardIs Enemies.theInescapable)
                ([FromEncounterDeck, FromEncounterDiscard] <> allOutOfPlayZones)
          addToVictory iid attrs
      pure t
    ForTarget (EnemyTarget enemy) (FailedThisSkillTest iid (isSource attrs -> True)) -> do
      enemyEngageInvestigator enemy iid
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      drawCard iid card
      pure t
    FoundEnemyInOutOfPlay _ iid (isTarget attrs -> True) enemy -> do
      card <- fetchCard enemy
      obtainCard card
      drawCard iid card
      pure t
    _ -> StillBehindYou <$> liftRunMessage msg attrs
