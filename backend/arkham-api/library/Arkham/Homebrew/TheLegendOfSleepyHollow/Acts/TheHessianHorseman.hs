module Arkham.Homebrew.TheLegendOfSleepyHollow.Acts.TheHessianHorseman (theHessianHorseman) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Scenario (getEncounterDeck, getEncounterDiscard)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (enemyMoveTo)
import Arkham.Placement
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (RegularEncounterDeck))

newtype TheHessianHorseman = TheHessianHorseman ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHessianHorseman :: ActCard TheHessianHorseman
theHessianHorseman = act (2, A) TheHessianHorseman Cards.theHessianHorseman Nothing

{- | "At the end of the round, if at least 2[per_investigator] of the following
are true, you may advance."
-}
instance HasAbilities TheHessianHorseman where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (RememberedAtLeast (PerPlayer 2) horsemansMemories)
      $ Objective
      $ freeReaction (RoundEnds #when)

instance RunMessage TheHessianHorseman where
  runMessage msg a@(TheHessianHorseman attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      investigators <- select UneliminatedInvestigator
      ichabod <- selectJust $ assetIs Assets.ichabodCrane
      chooseOneM lead $ scenarioI18n $ scope "theHessianHorseman" do
        questionLabeled "ichabod"
        portraits investigators (`takeControlOfAsset` ichabod)

      swamp <- selectJust $ locationIs Locations.wileysSwamp
      selectOne headlessHorseman >>= \case
        Just horseman -> enemyMoveTo attrs horseman swamp
        Nothing ->
          selectOne (OutOfPlayEnemy SetAsideZone headlessHorseman)
            >>= traverse_ (push . EnemySpawnFromOutOfPlay SetAsideZone Nothing swamp)

      -- "Search the encounter deck and discard pile for each copy of Burgeoning Evil"
      deck <- unDeck <$> getEncounterDeck
      encounterDiscard <- getEncounterDiscard RegularEncounterDeck
      let isBurgeoningEvil = (`cardMatch` cardIs Treacheries.burgeoningEvil) . toCard
      repeated (count isBurgeoningEvil (deck <> encounterDiscard))
        $ findEncounterCard lead attrs (cardIs Treacheries.burgeoningEvil)
      doStep 1 msg
      pure a
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) -> do
      horseman <- selectJust $ IncludeOutOfPlayEnemy headlessHorseman
      createTreacheryAt_ card (AttachedToEnemy horseman)
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      advanceActDeck attrs
      pure a
    _ -> TheHessianHorseman <$> liftRunMessage msg attrs
