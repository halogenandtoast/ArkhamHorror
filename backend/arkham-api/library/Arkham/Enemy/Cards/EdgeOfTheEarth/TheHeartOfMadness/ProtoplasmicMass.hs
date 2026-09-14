module Arkham.Enemy.Cards.EdgeOfTheEarth.TheHeartOfMadness.ProtoplasmicMass (protoplasmicMass) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.TheHeartOfMadness qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ProtoplasmicMass = ProtoplasmicMass EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protoplasmicMass :: EnemyCard ProtoplasmicMass
protoplasmicMass =
  enemy ProtoplasmicMass Cards.protoplasmicMass
    & setSpawnAt
      ( FarthestLocationFromYou
          $ mapOneOf LocationWithLabel ["facility1", "facility2", "facility3", "facility14", "facility15"]
      )
    & setOnlyPrey (mapOneOf InvestigatorWithSeal [minBound ..])

instance HasAbilities ProtoplasmicMass where
  getAbilities (ProtoplasmicMass a) =
    extend1 a
      $ restricted a 1 (exists $ mapOneOf InvestigatorWithActiveSeal [minBound ..])
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage ProtoplasmicMass where
  runMessage msg e@(ProtoplasmicMass attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      readyThis attrs
      resolveEnemyPhaseOf attrs
      pure e
    _ -> ProtoplasmicMass <$> liftRunMessage msg attrs
