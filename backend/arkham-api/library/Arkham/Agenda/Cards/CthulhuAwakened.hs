module Arkham.Agenda.Cards.CthulhuAwakened (cthulhuAwakened) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers
import Arkham.Trait (Trait (Artifact))

newtype CthulhuAwakened = CthulhuAwakened AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuAwakened :: AgendaCard CthulhuAwakened
cthulhuAwakened = agenda (2, A) CthulhuAwakened Cards.cthulhuAwakened (Static 100)

-- | Marker placed on an Artifact asset (round duration) once it has been used to
-- weaken Cthulhu this round, enforcing the "limit once per asset per round".
usedMarker :: ModifierType
usedMarker = ScenarioModifier "cthulhuAwakened.weakened"

-- | An Artifact asset controlled by the given investigator with doom that hasn't
-- yet weakened Cthulhu this round.
availableArtifact :: InvestigatorMatcher -> AssetMatcher
availableArtifact who =
  AssetWithTrait Artifact
    <> AssetControlledBy who
    <> AssetWithDoom (atLeast 1)
    <> AssetWithoutModifier usedMarker

instance HasAbilities CthulhuAwakened where
  getAbilities (CthulhuAwakened a) =
    [ mkAbility a 1 $ forced $ PlacedDoomCounter #after AnySource (targetIs a)
    , scenarioI18n
        $ withI18nTooltip "weakenCthulhu"
        $ restricted a 2 (DuringTurn You <> exists (availableArtifact You))
        $ FastAbility Free
    , mkAbility a 3 $ Objective $ forced $ ifEnemyDefeated Enemies.cthulhuDeadAndDreaming
    ]

instance RunMessage CthulhuAwakened where
  runMessage msg a@(CthulhuAwakened attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> directHorror iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assets <- select $ availableArtifact (InvestigatorWithId iid)
      chooseTargetM iid assets \aid -> do
        removeDoom (attrs.ability 2) aid 1
        -- Mark this asset as used for the rest of the round.
        roundModifier (attrs.ability 2) aid usedMarker
        selectForMaybeM (enemyIs Enemies.cthulhuDeadAndDreaming) \cthulhu ->
          roundModifiers
            (attrs.ability 2)
            cthulhu
            [RemoveKeyword Keyword.Relentless, EnemyFight (-1), EnemyEvade (-1)]
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      push R1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Each surviving investigator is driven insane; the scenario's No Resolution
      -- records Cthulhu annihilating the expedition.
      eachInvestigator drivenInsane
      noResolution
      pure a
    _ -> CthulhuAwakened <$> liftRunMessage msg attrs
