module Arkham.Agenda.Cards.TheFinalSeal (theFinalSeal) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Token (Token (Resource))
import Arkham.Trait (Trait (Artifact, Ruined))

newtype TheFinalSeal = TheFinalSeal AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalSeal :: AgendaCard TheFinalSeal
theFinalSeal = agenda (2, A) TheFinalSeal Cards.theFinalSeal (Static 10)

-- | A location that has had a sigil (a resource token) placed on it by the
-- ritual action below.
locationWithSigil :: LocationMatcher
locationWithSigil = LocationWithToken Resource

instance HasAbilities TheFinalSeal where
  getAbilities (TheFinalSeal a) =
    [ -- [action] Spend 1 clue (per investigator, as a group) AND place an
      -- Artifact asset you control in the victory display: place 1 sigil (a
      -- resource) on your location.
      scenarioI18n
        $ withI18nTooltip "placeSigil"
        $ restricted a 1 (exists $ AssetWithTrait Artifact <> AssetControlledBy You)
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
    , -- [Forced] When the enemy phase ends: draw the top card of the Cthulhu
      -- deck. (TODO: Cthulhu deck has no engine support yet.)
      mkAbility a 2 $ forced $ PhaseEnds #when #enemy
    , -- Objective: if each Cthulhu enemy is in the victory display and there are
      -- 5 locations with sigils, immediately advance.
      -- TODO: the "each Cthulhu facet is in the victory display" half is a
      -- Cthulhu Board interaction with no engine support; only the 5-sigil half
      -- of the condition is gated here.
      restricted a 3 (LocationCount 5 locationWithSigil)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheFinalSeal where
  runMessage msg a@(TheFinalSeal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      artifacts <- select $ AssetWithTrait Artifact <> AssetControlledBy (InvestigatorWithId iid)
      chooseTargetM iid artifacts \aid -> do
        addToVictory iid aid
        withLocationOf iid \lid -> placeTokens (attrs.ability 1) lid Resource 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- TODO: draw the top card of the Cthulhu deck and resolve it (no engine
      -- support for the Cthulhu deck of action cards yet).
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      -- TODO: the real objective also requires every Cthulhu facet to have been
      -- banished to the victory display (a Cthulhu Board interaction with no
      -- engine support). The 5-sigil half is gated by the ability restriction.
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) means -> do
      case means of
        AgendaAdvancedWithDoom ->
          -- The doom threshold was reached: Cthulhu's annihilating gaze sweeps
          -- away the expedition.
          selectEach UneliminatedInvestigator $ investigatorDefeated attrs
        _ -> do
          ruined <- selectCount $ LocationWithTrait Ruined
          if ruined <= 5 then push R2 else push R3
      pure a
    _ -> TheFinalSeal <$> liftRunMessage msg attrs
