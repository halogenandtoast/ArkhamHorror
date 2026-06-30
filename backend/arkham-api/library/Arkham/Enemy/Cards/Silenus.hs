module Arkham.Enemy.Cards.Silenus (silenus) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype Silenus = Silenus EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silenus :: EnemyCard Silenus
silenus = enemy Silenus Cards.silenus

instance HasModifiersFor Silenus where
  getModifiersFor (Silenus a) = do
    n <- perPlayer 8
    modifySelf a [HealthModifier n]

instance HasAbilities Silenus where
  getAbilities (Silenus a) =
    extend1 a
      $ restricted a 1 (exists $ investigatorAt (locationWithEnemy a))
      $ forced
      $ PhaseEnds #when #mythos

instance RunMessage Silenus where
  runMessage msg e@(Silenus attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt (locationWithEnemy attrs)) \iid ->
        forTarget iid msg
      pure e
    ForTarget (InvestigatorTarget iid) (UseThisAbility _ (isSource attrs -> True) 1) -> do
      hand <- iid.hand
      scenarioI18n $ blueDecide iid do
        labeled' "placeDoomOnTheBlueAgenda" $ placeDoomOnFactionAgenda (attrs.ability 1) BlueFaction 1
        labeled' "removeHandFromGame" $ for_ hand removeCardFromGame
        labeled' "take3Horror" $ assignHorror iid (attrs.ability 1) 3
      pure e
    _ -> Silenus <$> liftRunMessage msg attrs
