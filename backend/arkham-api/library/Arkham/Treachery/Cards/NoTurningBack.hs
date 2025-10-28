module Arkham.Treachery.Cards.NoTurningBack (noTurningBack) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.ForMovement
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NoTurningBack = NoTurningBack TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noTurningBack :: TreacheryCard NoTurningBack
noTurningBack = treachery NoTurningBack Cards.noTurningBack

instance HasModifiersFor NoTurningBack where
  getModifiersFor (NoTurningBack attrs) = case attrs.placement.attachedTo of
    Just (LocationTarget lid) -> do
      modifySelectMaybe attrs Anyone \iid -> do
        onNoTurningBack <- iid <=~> investigatorAt lid
        pure [if onNoTurningBack then CannotMove else CannotEnter lid]
    _ -> pure ()

instance HasAbilities NoTurningBack where
  getAbilities (NoTurningBack a) =
    [ skillTestAbility
        $ restricted
          a
          1
          (OnLocation $ orConnected NotForMovement $ LocationWithTreachery (be a))
          actionAbility
    ]

instance RunMessage NoTurningBack where
  runMessage msg t@(NoTurningBack attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      xs <-
        select
          $ LocationMatchAny
            [ locationWithInvestigator iid
            , connectedFrom (locationWithInvestigator iid)
            ]
          <> LocationWithoutTreachery (treacheryIs Cards.noTurningBack)
      chooseOrRunOneM iid $ targets xs $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasPickaxe <- getHasSupply iid Pickaxe
      sid <- getRandom
      chooseOrRunOneM iid do
        withI18n $ chooseTest #combat 3 $ beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 3)
        when hasPickaxe
          $ campaignI18n
          $ labeled' "checkYourSupplies"
          $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> NoTurningBack <$> liftRunMessage msg attrs
