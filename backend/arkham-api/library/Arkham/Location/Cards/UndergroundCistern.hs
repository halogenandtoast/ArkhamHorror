module Arkham.Location.Cards.UndergroundCistern (undergroundCistern) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Cards (undergroundCistern)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Cultist))

newtype UndergroundCistern = UndergroundCistern LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundCistern :: LocationCard UndergroundCistern
undergroundCistern = location UndergroundCistern Cards.undergroundCistern 4 (PerPlayer 2)

instance HasModifiersFor UndergroundCistern where
  getModifiersFor (UndergroundCistern attrs) = do
    when attrs.unrevealed $ modifySelf attrs [AdditionalCostToEnter $ GroupClueCost (PerPlayer 3) YourLocation]

instance HasAbilities UndergroundCistern where
  getAbilities (UndergroundCistern a) =
    extendRevealed a
    [ 
      restricted a 1 (thisExists a LocationWithoutClues) $ forced AnyWindow
    , mkAbility a 2 $ forced $ PhaseEnds #when #investigation
    ]

instance RunMessage UndergroundCistern where
  runMessage msg l@(UndergroundCistern attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 2
      placeClues (attrs.ability 1) attrs n
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      n <- selectCount $ EnemyAt (be attrs) <> ReadyEnemy <> NonEliteEnemy <> EnemyWithTrait Cultist
      when (n > 0) do
        discardEach attrs $ EnemyAt (be attrs) <> ReadyEnemy <> NonEliteEnemy <> EnemyWithTrait Cultist
        placeDoomOnAgenda n
      pure l
    _ -> UndergroundCistern <$> liftRunMessage msg attrs
