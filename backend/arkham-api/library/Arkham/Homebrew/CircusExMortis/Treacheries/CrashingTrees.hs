module Arkham.Homebrew.CircusExMortis.Treacheries.CrashingTrees (
  crashingTrees,
  forestRevelation,
  forestFailure,
) where

import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Scenario (getEncounterDiscard)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens, scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Queue (QueueT)
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (..))
import Arkham.SkillType (SkillType)
import Arkham.Treachery.Import.Lifted

newtype CrashingTrees = CrashingTrees TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crashingTrees :: TreacheryCard CrashingTrees
crashingTrees = treachery CrashingTrees Cards.crashingTrees

{- | Crashing Trees and Silent Forest are the same card but for the skill tested and
whether the failure penalty is damage or horror, so both halves live here.

Both riders are snapshotted from the ☾ tokens sealed when the test is initiated: the
treachery is discarded on resolution, so there is no entity left to re-read the count.
-}
forestRevelation :: ReverseQueue m => TreacheryAttrs -> InvestigatorId -> SkillType -> m ()
forestRevelation attrs iid sType = do
  sid <- getRandom
  moons <- length <$> getSealedMoonTokens iid
  when (moons >= 1)
    $ skillTestModifier sid attrs iid
    $ CannotTriggerAbilityMatching
    $ oneOf [AbilityIsFastAbility, AbilityIsReactionAbility]
  -- the commit gate reads each committing investigator's own modifiers, never the test's,
  -- so a ban on the test has to land on everyone who could commit to it
  when (moons >= 2) $ selectEach Anyone \other ->
    skillTestModifier sid attrs other (CannotCommitCards $ not_ WeaknessCard)
  revelationSkillTest sid iid attrs sType (Fixed 3)

{- | "you must either spawn the topmost enemy in the encounter discard pile at your
location, or take 1 damage/horror for each point you fail by"
-}
forestFailure
  :: ReverseQueue m
  => InvestigatorId
  -> Scope
  -- ^ the card's own i18n scope under @redSunrise@
  -> Int
  -- ^ the amount failed by
  -> Text
  -- ^ label key for the damage/horror option
  -> QueueT Message m ()
  -> m ()
forestFailure iid cardScope n penalty takePenalty = do
  topmost <- take 1 . filter (`cardMatch` card_ #enemy) <$> getEncounterDiscard RegularEncounterDeck
  scenarioI18n "redSunrise" $ scope cardScope $ countVar n $ chooseOneM iid do
    for_ topmost $ labeled "spawnEnemy" . withLocationOf iid . createEnemyAt_
    labeled penalty takePenalty

instance RunMessage CrashingTrees where
  runMessage msg t@(CrashingTrees attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      forestRevelation attrs iid #agility
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      forestFailure iid "crashingTrees" n "takeDamage" (assignDamage iid attrs n)
      pure t
    _ -> CrashingTrees <$> liftRunMessage msg attrs
