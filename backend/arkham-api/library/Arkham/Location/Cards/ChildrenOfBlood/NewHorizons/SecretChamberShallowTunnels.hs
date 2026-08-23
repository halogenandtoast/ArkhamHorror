module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.SecretChamberShallowTunnels (
  secretChamberShallowTunnels,
) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Enemies
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount, getSetAsideCardsMatching)
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SecretChamberShallowTunnels = SecretChamberShallowTunnels LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretChamberShallowTunnels :: LocationCard SecretChamberShallowTunnels
secretChamberShallowTunnels =
  symbolLabel
    $ location SecretChamberShallowTunnels Cards.secretChamberShallowTunnels 3 (PerPlayer 1)

instance HasModifiersFor SecretChamberShallowTunnels where
  getModifiersFor (SecretChamberShallowTunnels a) = unless a.revealed do
    modifySelf
      a
      [ AdditionalCostToEnter
          $ GroupClueCost (PerPlayer 1) (locationIs Cards.cavernEntranceShallowTunnels)
      ]

instance HasAbilities SecretChamberShallowTunnels where
  getAbilities (SecretChamberShallowTunnels a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage SecretChamberShallowTunnels where
  runMessage msg l@(SecretChamberShallowTunnels attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- getPlayerCount
      workers <- getSetAsideCardsMatching (cardIs Enemies.blightedWorker)
      let (spawning, remaining) = splitAt (if n >= 3 then 2 else 1) workers
      for_ spawning \card -> createEnemyWith_ card attrs.id createExhausted
      shuffleCardsIntoDeck Deck.EncounterDeck remaining
      pure l
    _ -> SecretChamberShallowTunnels <$> liftRunMessage msg attrs
