module Arkham.Asset.Assets.BookOfPsalms (bookOfPsalms) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BookOfPsalms = BookOfPsalms AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfPsalms :: AssetCard BookOfPsalms
bookOfPsalms = asset BookOfPsalms Cards.bookOfPsalms

instance HasAbilities BookOfPsalms where
  getAbilities (BookOfPsalms x) =
    [ controlled
        x
        1
        ( oneOf
            [ exists (HealableInvestigator (x.ability 1) #horror $ colocatedWithMatch You)
            , HasRemainingBlessTokens
            ]
        )
        (actionAbilityWithCost $ assetUseCost x Secret 1)
    ]

instance RunMessage BookOfPsalms where
  runMessage msg a@(BookOfPsalms attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror (colocatedWith iid)
      chooseTargetM iid investigators \investigator -> healHorror investigator (attrs.ability 1) 1
      n <- min 2 <$> getRemainingBlessTokens
      repeated n $ addChaosToken #bless
      pure a
    _ -> BookOfPsalms <$> liftRunMessage msg attrs
