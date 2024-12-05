module Arkham.Enemy.Cards.MindlessDancer (mindlessDancer, MindlessDancer (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Label
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Window qualified as Window

newtype MindlessDancer = MindlessDancer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindlessDancer :: EnemyCard MindlessDancer
mindlessDancer =
  enemyWith MindlessDancer Cards.mindlessDancer (6, Static 5, 3) (2, 1)
    $ spawnAtL
    ?~ SpawnAt
      (IncludeEmptySpace $ FarthestLocationFromYou $ locationIs Locations.emptySpace)

instance HasAbilities MindlessDancer where
  getAbilities (MindlessDancer a) =
    extend1 a
      $ groupLimit PerRound
      $ mkAbility a 1
      $ forced
      $ EnemyMovedTo #after (IncludeEmptySpace $ locationIs Locations.emptySpace) MovedViaHunter (be a)

instance HasModifiersFor MindlessDancer where
  getModifiersFor (MindlessDancer attrs) = modifySelf attrs [CanEnterEmptySpace]

instance RunMessage MindlessDancer where
  runMessage msg e@(MindlessDancer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      checkWhen $ Window.MovedFromHunter attrs.id
      push $ HunterMove (toId attrs)
      pure e
    PlaceCosmos _ lid (CosmosLocation pos _) -> do
      emptySpace <- selectJust $ IncludeEmptySpace $ LocationWithLabel (mkLabel $ cosmicLabel pos)
      case attrs.placement of
        AtLocation lid' | lid' == emptySpace -> do
          pure $ MindlessDancer $ attrs & placementL .~ AtLocation lid
        _ -> pure e
    _ -> MindlessDancer <$> liftRunMessage msg attrs
