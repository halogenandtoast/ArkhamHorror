module Arkham.Homebrew.DarkMatter.Treacheries.CabinPressure (cabinPressure) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype CabinPressure = CabinPressure TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cabinPressure :: TreacheryCard CabinPressure
cabinPressure = treachery CabinPressure Cards.cabinPressure

{- | "Hidden. Peril. Revelation - Secretly add this card to your hand.
Forced - At the end of your turn, if there are 1 or more clues on your
location: Take 2 damage and discard Cabin Pressure.
[action][action]: Discard Cabin Pressure."
-}
instance HasAbilities CabinPressure where
  getAbilities (CabinPressure a) =
    [ restricted a 1 (youExist $ at_ LocationWithAnyClues) $ forced $ TurnEnds #when You
    , restricted a 2 OnSameLocation $ doubleActionAbilityWithCost mempty
    ]

instance RunMessage CabinPressure where
  runMessage msg t@(CabinPressure attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 2
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> CabinPressure <$> liftRunMessage msg attrs
