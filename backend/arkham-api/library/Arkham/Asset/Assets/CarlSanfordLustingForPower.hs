module Arkham.Asset.Assets.CarlSanfordLustingForPower (carlSanfordLustingForPower) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import Arkham.Slot

newtype CarlSanfordLustingForPower = CarlSanfordLustingForPower AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carlSanfordLustingForPower :: AssetCard CarlSanfordLustingForPower
carlSanfordLustingForPower =
  allyWith CarlSanfordLustingForPower Cards.carlSanfordLustingForPower (3, 3) noSlots

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) []

instance HasModifiersFor CarlSanfordLustingForPower where
  getModifiersFor (CarlSanfordLustingForPower a) =
    controllerGets a [SkillModifier #willpower 1, AdditionalSlot #arcane]

instance HasAbilities CarlSanfordLustingForPower where
  getAbilities (CarlSanfordLustingForPower a) =
    [ reaction a 1 ControlsThis (exhaust a) $ CancelledOrIgnoredCardOrGameEffect AnySource
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage CarlSanfordLustingForPower where
  runMessage msg a@(CarlSanfordLustingForPower attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid card | card.id == attrs.cardId -> do
      push $ AddSlot iid #arcane (slot attrs)
      CarlSanfordLustingForPower <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach ActiveCard \card -> do
        when (cardMatch card (card_ $ #treachery <> WeaknessCard)) do
          mTid <- selectOne $ TreacheryWithCardId card.id
          for_ mTid \tid -> do
            addToVictory tid
            drawCards iid (attrs.ability 1) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure a
    _ -> CarlSanfordLustingForPower <$> liftRunMessage msg attrs
