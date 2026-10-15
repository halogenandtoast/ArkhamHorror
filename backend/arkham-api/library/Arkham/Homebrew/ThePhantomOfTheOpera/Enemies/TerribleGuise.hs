module Arkham.Homebrew.ThePhantomOfTheOpera.Enemies.TerribleGuise (terribleGuise) where

import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfMaybe)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype TerribleGuise = TerribleGuise EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

terribleGuise :: EnemyCard TerribleGuise
terribleGuise = enemyWith TerribleGuise Cards.terribleGuise (spawnAtL ?~ NoSpawn)

instance HasModifiersFor TerribleGuise where
  getModifiersFor (TerribleGuise a) = modifySelfMaybe a do
    lid <- MaybeT $ field EnemyLocation a.id
    n <- lift $ concealedCountAt lid
    pure [EnemyFight n]

instance RunMessage TerribleGuise where
  runMessage msg e@(TerribleGuise attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      let connecting = connectedFrom (locationWithInvestigator iid)
      canSpawnAtConnecting <- selectAny connecting
      chooseOneM iid $ scenarioI18n $ scope "terribleGuise" do
        when canSpawnAtConnecting do
          labeled "spawnAtConnecting"
            $ push
            $ EnemySpawnAtLocationMatching (Just iid) connecting attrs.id
        labeled "takeHorrorAndSpawnEngaged" do
          assignHorror iid attrs 1
          push $ EnemySpawnEngagedWith attrs.id (InvestigatorWithId iid)
      pure e
    _ -> TerribleGuise <$> liftRunMessage msg attrs
