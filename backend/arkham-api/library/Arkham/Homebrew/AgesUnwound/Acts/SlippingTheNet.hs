module Arkham.Homebrew.AgesUnwound.Acts.SlippingTheNet (slippingTheNet) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations (aPlaceToHide, crampedPassage, winchmoreHouse)
import Arkham.Matcher

newtype SlippingTheNet = SlippingTheNet ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slippingTheNet :: ActCard SlippingTheNet
slippingTheNet = act (3, A) SlippingTheNet Cards.slippingTheNet Nothing

instance HasModifiersFor SlippingTheNet where
  getModifiersFor (SlippingTheNet a) = modifySelect a Anywhere [ShroudModifier 1]

instance HasAbilities SlippingTheNet where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        ( notExists
            $ LocationWithAnyClues
            <> mapOneOf locationIs [aPlaceToHide, winchmoreHouse, crampedPassage]
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SlippingTheNet where
  runMessage msg a@(SlippingTheNet attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R4
      pure a
    _ -> SlippingTheNet <$> liftRunMessage msg attrs
