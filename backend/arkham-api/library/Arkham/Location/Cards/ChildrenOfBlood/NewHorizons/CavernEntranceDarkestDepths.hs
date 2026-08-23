module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.CavernEntranceDarkestDepths (cavernEntranceDarkestDepths) where

import Arkham.Ability
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CavernEntranceDarkestDepths = CavernEntranceDarkestDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernEntranceDarkestDepths :: LocationCard CavernEntranceDarkestDepths
cavernEntranceDarkestDepths =
  symbolLabel
    $ location CavernEntranceDarkestDepths Cards.cavernEntranceDarkestDepths 2 (PerPlayer 1)

instance HasAbilities CavernEntranceDarkestDepths where
  getAbilities (CavernEntranceDarkestDepths a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (SetAsideCardMatch $ CardWithTitle "Side Chamber"))
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage CavernEntranceDarkestDepths where
  runMessage msg l@(CavernEntranceDarkestDepths attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      sideChambers <- getSetAsideCardsMatching (CardWithTitle "Side Chamber")
      for_ (nonEmpty sideChambers) \xs -> do
        card <- sample xs
        placeLabeledLocations_ "sideChamber" [card]
      pure l
    _ -> CavernEntranceDarkestDepths <$> liftRunMessage msg attrs
