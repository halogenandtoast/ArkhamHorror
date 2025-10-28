module Arkham.Enemy.Cards.SpectralRaven (spectralRaven) where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier qualified as Mod
import Arkham.Phase
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype SpectralRaven = SpectralRaven EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRaven :: EnemyCard SpectralRaven
spectralRaven =
  enemy SpectralRaven Cards.spectralRaven (2, Static 2, 2) (1, 1)
    & setPrey (InvestigatorWithLowestSkill #intellect UneliminatedInvestigator)

instance HasAbilities SpectralRaven where
  getAbilities (SpectralRaven a) = extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

instance RunMessage SpectralRaven where
  runMessage msg e@(SpectralRaven attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasHauntedAbilities <- selectAny $ locationWithInvestigator iid <> HauntedLocation
      chooseOrRunOneM iid $ scenarioI18n do
        when hasHauntedAbilities do
          labeled' "spectralRaven.haunted" do
            handleTarget iid attrs attrs
          labeled' "spectralRaven.modify" do
            nextPhaseModifiers InvestigationPhase attrs attrs [Mod.EnemyFight 2, Mod.EnemyEvade 2]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) _ -> do
      runHauntedAbilities iid
      pure e
    _ -> SpectralRaven <$> liftRunMessage msg attrs
