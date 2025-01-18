module Arkham.Asset.Assets.InnocentReveler (innocentReveler) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher

newtype InnocentReveler = InnocentReveler AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innocentReveler :: AssetCard InnocentReveler
innocentReveler = allyWith InnocentReveler Cards.innocentReveler (2, 2) noSlots

instance HasAbilities InnocentReveler where
  getAbilities (InnocentReveler x) =
    [ skillTestAbility $ restricted x 1 (Uncontrolled <> OnSameLocation) parleyAction_
    , mkAbility x 2 $ forced $ AssetWouldBeDiscarded #when (be x)
    ]

instance RunMessage InnocentReveler where
  runMessage msg a@(InnocentReveler attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      let card = PlayerCard $ lookupPlayerCard (toCardDef attrs) (toCardId attrs)
      placeUnderneath AgendaDeckTarget [card]
      eachInvestigator \iid -> assignHorror iid (attrs.ability 2) 1
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfAsset iid attrs.id
      pure a
    _ -> InnocentReveler <$> liftRunMessage msg attrs
