module Arkham.Location.Cards.SigilCarvedAlcoveStoryOfTheVoyage (sigilCarvedAlcoveStoryOfTheVoyage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Projection ()
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SigilCarvedAlcoveStoryOfTheVoyage = SigilCarvedAlcoveStoryOfTheVoyage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sigilCarvedAlcoveStoryOfTheVoyage :: LocationCard SigilCarvedAlcoveStoryOfTheVoyage
sigilCarvedAlcoveStoryOfTheVoyage =
  location SigilCarvedAlcoveStoryOfTheVoyage Cards.sigilCarvedAlcoveStoryOfTheVoyage 4 (Static 1)

instance HasAbilities SigilCarvedAlcoveStoryOfTheVoyage where
  getAbilities (SigilCarvedAlcoveStoryOfTheVoyage a) =
    extendRevealed
      a
      [ -- [Forced] When Cthulhu enters this location: each investigator removes the
        -- top card of their deck from the game.
        mkAbility a 1 $ forced $ EnemyEnters #when (be a) (enemyIs Enemies.cthulhuDeadAndDreaming)
      , -- Alone among the alcoves this one has no skill test: the clue spend is the
        -- action's cost, and it buys the first doom on the Shard of Y'ch'lecht.
        restricted a 2 Here $ actionAbilityWithCost $ GroupClueCost (PerPlayer 1) (be a)
      ]

instance RunMessage SigilCarvedAlcoveStoryOfTheVoyage where
  runMessage msg l@(SigilCarvedAlcoveStoryOfTheVoyage attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        cards <- iid.topOfDeckN 1
        for_ cards removeCardFromGame
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      loadArtifact
        (attrs.ability 2)
        attrs
        iid
        Assets.shardOfYchlecht
        "placeAdditionalDoomOnShardOfYchlecht"
      pure l
    _ -> SigilCarvedAlcoveStoryOfTheVoyage <$> liftRunMessage msg attrs
