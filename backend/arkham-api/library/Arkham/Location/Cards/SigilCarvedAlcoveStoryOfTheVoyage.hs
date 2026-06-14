module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfTheVoyage (sigilCarvedAlcoveStoryOfTheVoyage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getLead)
import Arkham.Investigator.Projection ()
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SigilCarvedAlcoveStoryOfTheVoyage = SigilCarvedAlcoveStoryOfTheVoyage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfTheVoyage :: LocationCard SigilCarvedAlcoveStoryOfTheVoyage
sigilCarvedAlcoveStoryOfTheVoyage = location SigilCarvedAlcoveStoryOfTheVoyage Cards.sigilCarvedAlcoveStoryOfTheVoyage 4 (Static 1)

instance HasAbilities SigilCarvedAlcoveStoryOfTheVoyage where
  getAbilities (SigilCarvedAlcoveStoryOfTheVoyage a) =
    extendRevealed
      a
      [ -- [Forced] When Cthulhu enters this location: each investigator removes the
        -- top card of their deck from the game.
        mkAbility a 1 $ forced $ EnemyEnters #when (be a) (enemyIs Enemies.cthulhuDeadAndDreaming)
      , -- [action] Spend 1 [per_investigator] clues, as a group: place 1 doom on the
        -- Shard of Ychlecht (no skill test — the clue spend is the cost).
        restricted a 2 Here $ actionAbilityWithCost $ GroupClueCost (PerPlayer 1) (be a)
      ]

instance RunMessage SigilCarvedAlcoveStoryOfTheVoyage where
  runMessage msg l@(SigilCarvedAlcoveStoryOfTheVoyage attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        cards <- iid.topOfDeckN 1
        for_ cards removeCardFromGame
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      -- The clue cost above pays for the first doom.
      mShard <- selectOne (assetIs Assets.shardOfYchlecht)
      for_ mShard \shard -> do
        placeDoom (attrs.ability 2) shard 1
        -- Investigators here may spend 1 [per_investigator] more clues, as a group, to
        -- place 1 additional doom on it.
        investigators <- select (investigatorAt attrs)
        n <- getSpendableClueCount investigators
        x <- perPlayer 1
        when (n >= x) do
          lead <- getLead
          chooseOneM lead do
            labeled' "placeAdditionalDoomOnShardOfYchlecht" do
              spendCluesAsAGroup investigators x
              placeDoom (attrs.ability 2) shard 1
            labeled' "doNotPlaceAdditionalDoom" nothing
      pure l
    _ -> SigilCarvedAlcoveStoryOfTheVoyage <$> liftRunMessage msg attrs
