module Arkham.Enemy.Cards.SpectralRaven (spectralRaven, SpectralRaven (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier qualified as Mod
import Arkham.Phase

newtype SpectralRaven = SpectralRaven EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRaven :: EnemyCard SpectralRaven
spectralRaven =
  enemyWith SpectralRaven Cards.spectralRaven (2, Static 2, 2) (1, 1)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #intellect UneliminatedInvestigator)

instance HasAbilities SpectralRaven where
  getAbilities (SpectralRaven a) = extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

instance RunMessage SpectralRaven where
  runMessage msg e@(SpectralRaven attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasHauntedAbilities <- selectAny $ locationWithInvestigator iid <> HauntedLocation
      chooseOrRunOneM iid do
        when hasHauntedAbilities do
          labeled "Resolve each haunted ability on your location" do
            handleTarget iid attrs attrs
          labeled "Spectral Raven gets +2 fight and +2 evade until the end of the investigation phase" do
            nextPhaseModifiers InvestigationPhase attrs attrs [Mod.EnemyFight 2, Mod.EnemyEvade 2]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) _ -> do
      lift $ runHauntedAbilities iid
      pure e
    _ -> SpectralRaven <$> liftRunMessage msg attrs
