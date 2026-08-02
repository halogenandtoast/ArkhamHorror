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
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encroachingStorms :: AgendaCard EncroachingStorms
encroachingStorms = agenda (1, A) EncroachingStorms Cards.encroachingStorms (Static 3)

instance HasModifiersFor EncroachingStorms where
  getModifiersFor (EncroachingStorms a) = when (onSide A a) $ obsidianSkylineRules a

instance HasAbilities EncroachingStorms where
  getAbilities (EncroachingStorms a) =
    -- Forced - When an investigator's location would leave play: Move that
    -- investigator to any [[Central]] location. They take 2 direct damage. This
    -- is what keeps the winds and the act rebuilds from simply defeating anyone
    -- standing on a swept location.
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
          -- "Flip this agenda": the storm keeps building until it hits 6.
          push $ RevertAgenda attrs.id
          pure $ overAttrs (setMeta $ (storm + 1) <= 3) a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      let spawnExhausted = getMetaDefault False attrs
      withLocationOf iid \lid ->
        void $ createEnemyAtEdit ec lid (if spawnExhausted then createExhausted else id)
      pure a
    _ -> EncroachingStorms <$> liftRunMessage msg attrs
