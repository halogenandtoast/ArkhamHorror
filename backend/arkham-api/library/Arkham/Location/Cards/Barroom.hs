module Arkham.Location.Cards.Barroom (barroom) where

import Arkham.Ability
import Arkham.Helpers.Healing
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Barroom = Barroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barroom :: LocationCard Barroom
barroom = location Barroom Cards.barroom 3 (PerPlayer 2)

instance HasAbilities Barroom where
  getAbilities (Barroom a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted
        a
        1
        ( Here
            <> oneOf
              [ exists
                  $ oneOf
                    [ HealableInvestigator (a.ability 1) kind (affectsOthers $ InvestigatorAt YourLocation)
                    | kind <- [#damage, #horror]
                    ]
              , exists
                  $ oneOf
                    [HealableAsset (a.ability 1) kind (AssetAt YourLocation) | kind <- [#damage, #horror]]
              ]
        )
      $ FastAbility (ResourceCost 1)

instance RunMessage Barroom where
  runMessage msg l@(Barroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <-
        select
          $ oneOf
            [ HealableInvestigator (attrs.ability 1) kind (affectsOthers $ at_ $ locationWithInvestigator iid)
            | kind <- [#damage, #horror]
            ]
      assets <-
        select
          $ oneOf
            [ HealableAsset (attrs.ability 1) kind (at_ $ locationWithInvestigator iid)
            | kind <- [#damage, #horror]
            ]
      chooseOneM iid do
        targets investigators $ chooseHealDamageOrHorrorOn (attrs.ability 1) iid
        targets assets $ assetChooseHealDamageOrHorror (attrs.ability 1) iid
      pure l
    _ -> Barroom <$> liftRunMessage msg attrs
