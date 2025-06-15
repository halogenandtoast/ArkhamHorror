module Arkham.Asset.Assets.CarlSanfordLustingForPower (
  carlSanfordLustingForPower,
  CarlSanfordLustingForPower(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Slot
import Arkham.Card (toCardId)
import Arkham.Card.CardDef (toCardDef, cdCardType, cdCardSubType)
import Arkham.Card.CardType

newtype CarlSanfordLustingForPower = CarlSanfordLustingForPower AssetAttrs
  deriving anyclass (IsAsset)
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
    [ reaction a 1 ControlsThis (exhaust a) $
        CancelledOrIgnoredCardOrGameEffect AnySource
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage CarlSanfordLustingForPower where
  runMessage msg a@(CarlSanfordLustingForPower attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid #arcane (slot attrs)
      CarlSanfordLustingForPower <$> liftRunMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      game <- getGame
      for_ game.activeCard \card -> do
        let def = toCardDef card
        when (cdCardType def == TreacheryType && cdCardSubType def /= Just Weakness && cdCardSubType def /= Just BasicWeakness) do
          mTid <- selectOne $ TreacheryWithCardId (toCardId card)
          for_ mTid \tid -> do
            addToVictory (attrs.ability 1) tid
            drawCards iid (attrs.ability 1) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure a
    _ -> CarlSanfordLustingForPower <$> liftRunMessage msg attrs
