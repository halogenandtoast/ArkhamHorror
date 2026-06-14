module Arkham.Agenda.Cards.EncroachingStorms (encroachingStorms) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window (locationLeavingPlay)
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Trait (Trait (Central))

newtype EncroachingStorms = EncroachingStorms AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encroachingStorms :: AgendaCard EncroachingStorms
encroachingStorms = agenda (1, A) EncroachingStorms Cards.encroachingStorms (Static 3)

-- | Side A statics that have no engine support yet (they depend on the
-- "open sky" / Summit-deck / sliding-location infrastructure):
--   * "Each non-weakness enemy may enter or leave open sky as if it were a
--     location."
--   * "Each location is connected to each location (and open sky) adjacent to
--     it."
-- TODO: implement these once open sky / sliding locations exist (HasModifiersFor).
instance HasAbilities EncroachingStorms where
  getAbilities (EncroachingStorms a) =
    -- Forced - When an investigator's location would leave play: Move that
    -- investigator to any [[Central]] location. They take 2 direct damage.
    -- The trigger only fires once locations actually leave play via the
    -- (not-yet-implemented) sliding mechanic, but the response itself maps to
    -- existing primitives, so we wire it up here.
    [ uncancellable (mkAbility a 1 $ forced $ LocationLeavesPlay #when (LocationWithInvestigator Anyone))
    | onSide A a
    ]

instance RunMessage EncroachingStorms where
  runMessage msg a@(EncroachingStorms attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (locationLeavingPlay -> lid) _ -> do
      selectEach (InvestigatorAt $ LocationWithId lid) \iid -> do
        moveToMatch (attrs.ability 1) iid (withTrait Central)
        directDamage iid (attrs.ability 1) 2
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      storm <- getStormIntensity
      if storm >= 6
        then do
          -- Each surviving investigator is defeated and suffers 1 physical trauma.
          selectEach UneliminatedInvestigator \iid -> do
            push $ InvestigatorDefeated (toSource attrs) iid
            sufferPhysicalTrauma iid 1
          pure a
        else do
          scenarioSpecific "increaseStormIntensity" ()
          -- Discard from the top of the encounter deck until an enemy is
          -- discarded and spawn it at the lead investigator's location. If the
          -- (post-increase) storm intensity is 3 or less, it enters exhausted
          -- (and therefore unengaged), which the RequestedEncounterCard handler
          -- reads back from this agenda's meta.
          lead <- getLead
          discardUntilFirst lead attrs Deck.EncounterDeck #enemy
          pure $ overAttrs (setMeta $ (storm + 1) <= 3) a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      let spawnExhausted = getMetaDefault False attrs
      withLocationOf iid \lid ->
        void $ createEnemyAtEdit ec lid (if spawnExhausted then createExhausted else id)
      pure a
    _ -> EncroachingStorms <$> liftRunMessage msg attrs
