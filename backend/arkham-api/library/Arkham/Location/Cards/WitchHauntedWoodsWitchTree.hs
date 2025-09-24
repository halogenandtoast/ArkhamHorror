module Arkham.Location.Cards.WitchHauntedWoodsWitchTree (witchHauntedWoodsWitchTree) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype WitchHauntedWoodsWitchTree = WitchHauntedWoodsWitchTree LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsWitchTree :: LocationCard WitchHauntedWoodsWitchTree
witchHauntedWoodsWitchTree = location WitchHauntedWoodsWitchTree Cards.witchHauntedWoodsWitchTree 4 (PerPlayer 1)

instance HasAbilities WitchHauntedWoodsWitchTree where
  getAbilities (WitchHauntedWoodsWitchTree a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (mapOneOf matcher [#damage, #horror]))
      $ FastAbility (DirectDamageAndHorrorCost (a.ability 1) You 1 1)
   where
    matcher kind = HealableInvestigator (a.ability 1) kind $ not_ You <> InvestigatorAt (not_ (be a))

instance RunMessage WitchHauntedWoodsWitchTree where
  runMessage msg l@(WitchHauntedWoodsWitchTree attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <-
        select
          $ oneOf
            [ HealableInvestigator (attrs.ability 1) #horror $ not_ (be iid) <> at_ (not_ (be attrs))
            , HealableInvestigator (attrs.ability 1) #damage $ not_ (be iid) <> at_ (not_ (be attrs))
            ]
      chooseTargetM iid investigators \iid' -> do
        healDamage iid' (attrs.ability 1) 1
        healHorror iid' (attrs.ability 1) 1
      pure l
    _ -> WitchHauntedWoodsWitchTree <$> liftRunMessage msg attrs
