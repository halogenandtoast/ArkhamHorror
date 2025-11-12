module Arkham.Location.Cards.BaccaratTable (baccaratTable) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype BaccaratTable = BaccaratTable LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baccaratTable :: LocationCard BaccaratTable
baccaratTable = symbolLabel $ location BaccaratTable Cards.baccaratTable 0 (Static 0)

instance HasAbilities BaccaratTable where
  getAbilities (BaccaratTable a) =
    extendRevealed1 a $ restricted a 1 Here $ actionAbilityWithCost (ResourceCost 2)

data BaccaratChoice = Player | Banker | Tie
  deriving stock (Show, Eq, Enum, Bounded)

instance RunMessage BaccaratTable where
  runMessage msg l@(BaccaratTable attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scenarioI18n
        $ chooseOneDropDown
          iid
          [ (ikey' ("label.baccarat." <> tshow suit), DoStep (fromEnum suit) msg)
          | suit <- [minBound @BaccaratChoice ..]
          ]
      pure l
    DoStep choice (UseThisAbility iid (isSource attrs -> True) 1) -> do
      checkGameIcons attrs iid NoMulligan 2
      pure $ BaccaratTable $ attrs & setMeta choice
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      let cards' = cards & mapMaybe \c -> (c,) <$> toPlayingCard c
      let choice = toEnum @BaccaratChoice $ toResult @Int attrs.meta
      let total = sum $ map (numericValue . snd) cards'
      let
        winner
          | total > 18 = Player
          | total < 18 = Banker
          | otherwise = Tie

      when (winner == choice) $ winGame iid attrs 4
      focusCards (map fst cards') $ continue_ iid
      pure l
    _ -> BaccaratTable <$> liftRunMessage msg attrs
