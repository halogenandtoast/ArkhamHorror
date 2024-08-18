module Arkham.Enemy.Cards.SpectralRaven (spectralRaven, SpectralRaven (..)) where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Modifier qualified as Mod
import Arkham.Phase
import Arkham.Prelude

newtype SpectralRaven = SpectralRaven EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRaven :: EnemyCard SpectralRaven
spectralRaven =
  enemyWith SpectralRaven Cards.spectralRaven (2, Static 2, 2) (1, 1)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #intellect UneliminatedInvestigator)

instance HasAbilities SpectralRaven where
  getAbilities (SpectralRaven a) =
    extend a [mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)]

instance RunMessage SpectralRaven where
  runMessage msg e@(SpectralRaven attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasHauntedAbilities <- selectAny $ locationWithInvestigator iid <> HauntedLocation
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Resolve each haunted ability on your location"
            [HandleTargetChoice iid (toSource attrs) (toTarget attrs)]
          | hasHauntedAbilities
          ]
        <> [ Label
              "Spectral Raven gets +2 fight and +2 evade until the end of the investigation phase"
              [ createWindowModifierEffect
                  (EffectPhaseWindowFor InvestigationPhase)
                  attrs
                  attrs
                  [Mod.EnemyFight 2, Mod.EnemyEvade 2]
              ]
           ]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) _ -> do
      runHauntedAbilities iid
      pure e
    _ -> SpectralRaven <$> runMessage msg attrs
