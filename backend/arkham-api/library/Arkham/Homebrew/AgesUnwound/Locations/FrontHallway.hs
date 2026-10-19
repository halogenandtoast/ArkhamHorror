module Arkham.Homebrew.AgesUnwound.Locations.FrontHallway (frontHallway) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FrontHallway = FrontHallway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frontHallway :: LocationCard FrontHallway
frontHallway =
  symbolLabel
    $ location FrontHallway Cards.frontHallway 3 (PerPlayer 2)
    & setCostToEnterUnrevealed (GroupClueCost (PerPlayer 3) "Children's Playground")

instance HasAbilities FrontHallway where
  getAbilities (FrontHallway a) =
    extendRevealed1 a
      $ campaignI18n
      $ withI18nTooltip "frontHallway.haunted"
      $ restricted a 1 (exists $ enemyIs Enemies.houndOfUnmaking <> EnemyWithoutDamage) Haunted

instance RunMessage FrontHallway where
  runMessage msg l@(FrontHallway attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withMatch (enemyIs Enemies.houndOfUnmaking <> EnemyWithoutDamage)
        $ placeTokensOn (attrs.ability 1) #damage 1
      pure l
    _ -> FrontHallway <$> liftRunMessage msg attrs
