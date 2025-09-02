module Arkham.Act.Cards.TheAbominableContessa (theAbominableContessa) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Scenario
import Arkham.Keyword (Keyword (Elusive))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Scenario.Deck
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Trait (Trait (Castle))

newtype TheAbominableContessa = TheAbominableContessa ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAbominableContessa :: ActCard TheAbominableContessa
theAbominableContessa = act (2, A) TheAbominableContessa Cards.theAbominableContessa Nothing

instance HasModifiersFor TheAbominableContessa where
  getModifiersFor (TheAbominableContessa a) = do
    modifySelect
      a
      (EnemyWithTitle "Possessed Extra")
      [EnemyFight 1, HealthModifier 1, AddKeyword Elusive]
    modifySelect a ExhaustedEnemy [CannotBeDamaged]

instance HasAbilities TheAbominableContessa where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ VictoryDisplayCardMatch $ basic $ cardIs Assets.accursedCapeShroudOfChaos)
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage TheAbominableContessa where
  runMessage msg a@(TheAbominableContessa attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      mcontessa <- selectOne $ InPlayEnemy $ enemyIs Enemies.theContessaNeedlesslySmug
      for_ mcontessa \contessa -> do
        healAllDamage attrs contessa
        place contessa $ OutOfPlay SetAsideZone
      discardEach attrs (not_ $ enemyIs Enemies.theContessaNeedlesslySmug)
      gothicSet <- selectJust $ locationIs Locations.gothicSet
      eachInvestigator \iid -> do
        moveTo attrs iid gothicSet
        discardAllClues attrs iid

      selectEach (LocationWithTrait Castle) removeLocation

      placeSetAsideLocations_ [Locations.spaceSet, Locations.jungleSet]
      push $ SetLayout initialLayout
      lead <- getLead
      centralLot <- placeSetAsideLocation Locations.centralLotQuietOnSet
      flipOverBy lead attrs centralLot

      contessa <-
        selectOne (enemyIs Enemies.theContessaNeedlesslySmug) >>= \case
          Nothing -> createEnemyAt Enemies.theContessaEnraged centralLot
          Just contessa -> do
            place contessa centralLot
            flipOverBy lead attrs contessa
            pure contessa

      cloak <- selectJust $ VictoryDisplayCardMatch $ basic $ cardIs Assets.accursedCapeShroudOfChaos
      createAssetAt_ cloak (AttachedToEnemy contessa)

      reelDeck <- take 2 <$> getScenarioDeck ReelDeck
      shuffleCardsIntoDeck Deck.EncounterDeck reelDeck
      shuffleEncounterDiscardBackIn

      addChaosToken #cultist
      addChaosToken #tablet
      addChaosToken #elderthing

      advanceActDeck attrs
      pure a
    _ -> TheAbominableContessa <$> liftRunMessage msg attrs
