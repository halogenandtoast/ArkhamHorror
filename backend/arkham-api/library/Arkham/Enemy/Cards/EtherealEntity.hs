module Arkham.Enemy.Cards.EtherealEntity (etherealEntity, etherealEntityWarOfTheOuterGods) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype EtherealEntity = EtherealEntity EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealEntity :: EnemyCard EtherealEntity
etherealEntity =
  enemyWith EtherealEntity Cards.etherealEntity
    $ spawnAtL
    ?~ SpawnAt (locationIs Locations.athenaeumOfTheEmptySky)

etherealEntityWarOfTheOuterGods :: EnemyCard EtherealEntity
etherealEntityWarOfTheOuterGods =
  enemyWith EtherealEntity Cards.etherealEntityWarOfTheOuterGods
    $ spawnAtL
    ?~ SpawnAt (locationIs Locations.athenaeumOfTheEmptySky)

instance HasAbilities EtherealEntity where
  getAbilities (EtherealEntity a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

instance RunMessage EtherealEntity where
  runMessage msg e@(EtherealEntity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      scenarioI18n $ blueDecide iid do
        labeled' "placeDoomOnTheBlueAgenda" $ placeDoomOnFactionAgenda (attrs.ability 1) BlueFaction 1
        when hasClues do
          labeled' "placeClueOnYourLocation" $ push $ InvestigatorPlaceCluesOnLocation iid (attrs.ability 1) 1
        labeled' "take2Horror" $ assignHorror iid (attrs.ability 1) 2
      pure e
    _ -> EtherealEntity <$> liftRunMessage msg attrs
