module Arkham.Asset.Assets.Kleptomania (kleptomania) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Kleptomania = Kleptomania AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kleptomania :: AssetCard Kleptomania
kleptomania = asset Kleptomania Cards.kleptomania

instance HasAbilities Kleptomania where
  getAbilities (Kleptomania a) =
    [ controlled
        a
        1
        ( atYourLocation
            $ affectsOthers
            $ NotYou
            <> oneOf [InvestigatorWithResources (atLeast 2), HasMatchingAsset #item]
        )
        actionAbility
    , restricted a 2 ControlsThis $ forced $ TurnEnds #when You
    ]

instance RunMessage Kleptomania where
  runMessage msg t@(Kleptomania attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ AssetOwnedBy (notInvestigator iid <> colocatedWith iid) <> #item
      investigators <-
        select $ notInvestigator iid <> colocatedWith iid <> InvestigatorWithResources (atLeast 2)
      chooseOneM iid do
        targets investigators \iid' -> do
          loseResources iid' (attrs.ability 1) 2
          takeResources iid iid' 2

        targets assets (takeControlOfAsset iid)
      shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 1
      pure t
    _ -> Kleptomania <$> liftRunMessage msg attrs
