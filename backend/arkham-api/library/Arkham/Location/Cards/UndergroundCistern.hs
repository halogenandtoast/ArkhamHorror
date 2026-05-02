module Arkham.Location.Cards.UndergroundCistern (undergroundCistern) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
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
    when attrs.unrevealed
      $ modifySelf attrs [AdditionalCostToEnter $ GroupClueCost (PerPlayer 3) YourLocation]

instance HasAbilities UndergroundCistern where
  getAbilities (UndergroundCistern a) =
    extendRevealed
      a
      [ restricted a 1 (thisExists a LocationWithoutClues) $ forced AnyWindow
      , restricted a 2 (exists $ enemyAt a <> NonEliteEnemy <> EnemyWithTrait Cultist)
          $ forced
          $ PhaseEnds #when #investigation
      ]

instance RunMessage UndergroundCistern where
  runMessage msg l@(UndergroundCistern attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs =<< perPlayer 2
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      enemies <- select $ EnemyAt (be attrs) <> ReadyEnemy <> NonEliteEnemy <> EnemyWithTrait Cultist
      unless (null enemies) do
        traverse_ (toDiscard (attrs.ability 2)) enemies
        placeDoomOnAgenda $ length enemies
      pure l
    _ -> UndergroundCistern <$> liftRunMessage msg attrs
