module Arkham.Location.Cards.WitchHauntedWoodsChildsTreeHouse (
  witchHauntedWoodsChildsTreeHouse,
) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WitchHauntedWoodsChildsTreeHouse = WitchHauntedWoodsChildsTreeHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsChildsTreeHouse
  :: LocationCard WitchHauntedWoodsChildsTreeHouse
witchHauntedWoodsChildsTreeHouse =
  location
    WitchHauntedWoodsChildsTreeHouse
    Cards.witchHauntedWoodsChildsTreeHouse
    1
    (PerPlayer 2)

instance HasAbilities WitchHauntedWoodsChildsTreeHouse where
  getAbilities (WitchHauntedWoodsChildsTreeHouse a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after Anyone (be a)

instance RunMessage WitchHauntedWoodsChildsTreeHouse where
  runMessage msg l@(WitchHauntedWoodsChildsTreeHouse attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mEnemy <- findTopOfDiscard #enemy
      lead <- getLead
      case mEnemy of
        Nothing -> discardUntilFirst lead attrs Deck.EncounterDeck #enemy
        Just enemy -> spawnEnemyAt_ enemy attrs
      pure l
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      spawnEnemyAt_ ec attrs
      pure l
    _ -> WitchHauntedWoodsChildsTreeHouse <$> liftRunMessage msg attrs
