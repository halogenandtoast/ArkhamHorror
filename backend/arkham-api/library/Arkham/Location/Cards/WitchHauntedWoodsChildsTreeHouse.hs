module Arkham.Location.Cards.WitchHauntedWoodsChildsTreeHouse (
  witchHauntedWoodsChildsTreeHouse,
  WitchHauntedWoodsChildsTreeHouse (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

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
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ DiscoveringLastClue Timing.After Anyone
          $ LocationWithId
          $ toId a
      ]

instance RunMessage WitchHauntedWoodsChildsTreeHouse where
  runMessage msg l@(WitchHauntedWoodsChildsTreeHouse attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mEnemy <- findTopOfDiscard $ CardWithType EnemyType
      lead <- getLead
      push $ case mEnemy of
        Nothing ->
          DiscardUntilFirst
            lead
            (toSource attrs)
            Deck.EncounterDeck
            (BasicCardMatch $ CardWithType EnemyType)
        Just enemy -> SpawnEnemyAt (EncounterCard enemy) (toId attrs)
      pure l
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      push $ SpawnEnemyAt (EncounterCard ec) (toId attrs)
      pure l
    _ -> WitchHauntedWoodsChildsTreeHouse <$> runMessage msg attrs
