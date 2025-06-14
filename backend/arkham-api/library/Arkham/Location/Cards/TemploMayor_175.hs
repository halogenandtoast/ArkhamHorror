module Arkham.Location.Cards.TemploMayor_175 (temploMayor_175) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TemploMayor_175 = TemploMayor_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temploMayor_175 :: LocationCard TemploMayor_175
temploMayor_175 = symbolLabel $ location TemploMayor_175 Cards.temploMayor_175 2 (PerPlayer 2)

instance HasAbilities TemploMayor_175 where
  getAbilities (TemploMayor_175 attrs) =
    extendRevealed
      attrs
      [ mkAbility attrs 1 $ forced $ PutLocationIntoPlay #after Anyone (be attrs)
      , groupLimit PerPhase
          $ restricted attrs 2 (CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (LocationWithId attrs.id))
          $ actionAbilityWithCost (ShuffleDiscardCost 1 WeaknessCard)
      ]

instance RunMessage TemploMayor_175 where
  runMessage msg l@(TemploMayor_175 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      shuffleDiscardBackIn iid
      discardUntilFirst iid (attrs.ability 1) (Deck.InvestigatorDeck iid) (basic WeaknessCard)
      pure l
    RequestedPlayerCard iid (isSource attrs -> True) mcard _ -> do
      for_ mcard $ addToHand iid . only
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAt NotInvestigate iid (attrs.ability 2) attrs 2
      pure l
    _ -> TemploMayor_175 <$> liftRunMessage msg attrs
