module Arkham.Homebrew.DarkMatter.Enemies.FlightOfTheByakhees (haita) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (AssetDefeated)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addImpendingDoom)
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain)
import Arkham.Matcher

newtype FlightOfTheByakhees = FlightOfTheByakhees EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haita :: EnemyCard FlightOfTheByakhees
haita = enemy FlightOfTheByakhees Cards.haita

{- | "Forced - When a [[Brain]] story asset is defeated: Remove it from the game
and add 1 tally mark under 'Impending Doom' in your Campaign Log."
-}
instance HasAbilities FlightOfTheByakhees where
  getAbilities (FlightOfTheByakhees a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ AssetDefeated #when ByAny (AssetWithTrait Brain)

instance RunMessage FlightOfTheByakhees where
  runMessage msg e@(FlightOfTheByakhees attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      brains <- select $ AssetWithTrait Brain <> AssetWithDamage
      for_ brains \aid -> push $ RemoveFromGame (toTarget aid)
      addImpendingDoom 1
      pure e
    _ -> FlightOfTheByakhees <$> liftRunMessage msg attrs
