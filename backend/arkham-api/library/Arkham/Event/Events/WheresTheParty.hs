module Arkham.Event.Events.WheresTheParty (wheresTheParty) where

import Arkham.Card
import Arkham.Enemy (lookupEnemy)
import Arkham.Enemy.Creation
import Arkham.Enemy.Types (EnemyAttrs (enemyHealthDamage, enemySanityDamage), Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Enemy
import Arkham.Matcher
import Arkham.Trait (Trait (Elite))

newtype WheresTheParty = WheresTheParty EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wheresTheParty :: EventCard WheresTheParty
wheresTheParty = event WheresTheParty Cards.wheresTheParty

instance RunMessage WheresTheParty where
  runMessage msg e@(WheresTheParty attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      findEncounterCard iid attrs $ card_ $ #enemy <> not_ (withTrait Elite)
      pure e
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyWith
        card
        iid
        (\ec -> ec {enemyCreationExhausted = True, enemyCreationInvestigator = Just iid})
        >>= (`forTarget` msg)
      pure e
    ForTarget (EnemyTarget enemy) (FoundEncounterCard iid (isTarget attrs -> True) card) -> do
      mhealth <- getEnemyField EnemyHealthDamage enemy
      mhorror <- getEnemyField EnemySanityDamage enemy
      let
        (health, horror) = case (mhealth, mhorror) of
          (Nothing, Nothing) ->
            let x = lookupEnemy card.cardCode enemy card.id
             in (attr enemyHealthDamage x, attr enemySanityDamage x)
          (_, _) -> (fromMaybe 0 mhealth, fromMaybe 0 mhorror)
      drawCards iid attrs (health + horror)
      pure e
    _ -> WheresTheParty <$> liftRunMessage msg attrs
