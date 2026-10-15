module Arkham.Homebrew.ThePhantomOfTheOpera.Enemies.ViolentShade (violentShade) where

import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfMaybe)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype ViolentShade = ViolentShade EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

violentShade :: EnemyCard ViolentShade
violentShade = enemyWith ViolentShade Cards.violentShade (spawnAtL ?~ NoSpawn)

instance HasModifiersFor ViolentShade where
  getModifiersFor (ViolentShade a) = modifySelfMaybe a do
    lid <- MaybeT $ field EnemyLocation a.id
    n <- lift $ concealedCountAt lid
    pure [EnemyEvade n]

instance RunMessage ViolentShade where
  runMessage msg e@(ViolentShade attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      let connecting = connectedFrom (locationWithInvestigator iid)
      canSpawnAtConnecting <- selectAny connecting
      chooseOneM iid $ scenarioI18n $ scope "violentShade" do
        when canSpawnAtConnecting do
          labeled "spawnAtConnecting"
            $ push
            $ EnemySpawnAtLocationMatching (Just iid) connecting attrs.id
        labeled "takeDamageAndSpawnEngaged" do
          assignDamage iid attrs 1
          push $ EnemySpawnEngagedWith attrs.id (InvestigatorWithId iid)
      pure e
    _ -> ViolentShade <$> liftRunMessage msg attrs
