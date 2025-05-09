module Arkham.Enemy.Cards.ProtoplasmicMass (protoplasmicMass) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ProtoplasmicMass = ProtoplasmicMass EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protoplasmicMass :: EnemyCard ProtoplasmicMass
protoplasmicMass =
  enemyWith ProtoplasmicMass Cards.protoplasmicMass (6, Static 4, 3) (1, 1)
    $ ( spawnAtL
          ?~ SpawnAt
            ( FarthestLocationFromYou
                $ mapOneOf LocationWithLabel ["facility1", "facility2", "facility3", "facility14", "facility15"]
            )
      )
    . (preyL .~ OnlyPrey (mapOneOf InvestigatorWithSeal [minBound ..]))

instance HasAbilities ProtoplasmicMass where
  getAbilities (ProtoplasmicMass a) =
    extend1 a
      $ restricted a 1 (exists $ mapOneOf InvestigatorWithActiveSeal [minBound ..])
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage ProtoplasmicMass where
  runMessage msg (ProtoplasmicMass attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      doStep 1 msg
      ProtoplasmicMass <$> liftRunMessage HuntersMove attrs
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      ProtoplasmicMass <$> liftRunMessage EnemiesAttack attrs
    _ -> ProtoplasmicMass <$> liftRunMessage msg attrs
