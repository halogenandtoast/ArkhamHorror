module Arkham.Treachery.Cards.MiasmaticTorment (miasmaticTorment) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Keyword (Keyword (Partner))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MiasmaticTorment = MiasmaticTorment TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miasmaticTorment :: TreacheryCard MiasmaticTorment
miasmaticTorment = treachery MiasmaticTorment Cards.miasmaticTorment

instance HasModifiersFor MiasmaticTorment where
  getModifiersFor (MiasmaticTorment a) = case a.placement of
    AttachedToAsset aid _ -> modified_ a aid [CannotReady]
    _ -> pure ()

instance HasAbilities MiasmaticTorment where
  getAbilities (MiasmaticTorment a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    , skillTestAbility $ restricted a 2 OnSameLocation actionAbility
    ]

instance RunMessage MiasmaticTorment where
  runMessage msg t@(MiasmaticTorment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      partners <- select $ AssetWithKeyword Partner
      if null partners
        then gainSurge attrs
        else chooseOneM iid $ targets partners \partner -> do
          exhaustThis partner
          attachTreachery attrs partner
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToAsset aid _ -> do
          push $ DealAssetDamageWithCheck aid (attrs.ability 1) 1 1 True
        _ -> pure ()
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 2) iid sType (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> MiasmaticTorment <$> liftRunMessage msg attrs
