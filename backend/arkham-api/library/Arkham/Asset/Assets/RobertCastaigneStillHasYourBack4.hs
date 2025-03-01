module Arkham.Asset.Assets.RobertCastaigneStillHasYourBack4 (robertCastaigneStillHasYourBack4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card.Id
import Arkham.Helpers.Ability (getCanPerformAbility)
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Message.Lifted.Choose
import Arkham.Window (defaultWindows)

newtype RobertCastaigneStillHasYourBack4 = RobertCastaigneStillHasYourBack4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

robertCastaigneStillHasYourBack4 :: AssetCard RobertCastaigneStillHasYourBack4
robertCastaigneStillHasYourBack4 = ally RobertCastaigneStillHasYourBack4 Cards.robertCastaigneStillHasYourBack4 (2, 2)

instance HasModifiersFor RobertCastaigneStillHasYourBack4 where
  getModifiersFor (RobertCastaigneStillHasYourBack4 a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities RobertCastaigneStillHasYourBack4 where
  getAbilities (RobertCastaigneStillHasYourBack4 a) =
    [ controlled
        a
        1
        ( exists
            $ InHandOf NotForPlay You
            <> basic (#firearm <> CardFillsLessSlots 2 #hand)
            <> CardWithPerformableAbility #fight [IgnoreAllCosts]
        )
        $ FastAbility' (exhaust a) [#fight]
    ]

instance RunMessage RobertCastaigneStillHasYourBack4 where
  runMessage msg a@(RobertCastaigneStillHasYourBack4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <-
        select
          $ inHandOf NotForPlay iid
          <> basic (#firearm <> CardFillsLessSlots 2 #hand)
          <> CardWithPerformableAbility #fight [IgnoreAllCosts]
      focusCards cards do
        chooseTargetM iid cards \card -> do
          withCardEntity card $ handleTarget iid attrs (AssetId $ unsafeCardIdToUUID card.id)
          handleTarget iid attrs card
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      let
        adjustAbility ab =
          applyAbilityModifiers
            (ab {abilityDoesNotProvokeAttacksOfOpportunity = True})
            [IgnoreAllCosts]
      abilities <-
        filterM (getCanPerformAbility iid (defaultWindows iid))
          =<< selectMap adjustAbility (AssetAbility (AssetWithId aid) <> #fight)
      chooseOneM iid $ for_ abilities \ab -> abilityLabeled iid ab nothing
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      inHand <- selectAny $ inHandOf NotForPlay iid
      when inHand do
        card <- getCard cid
        canPlay <- getIsPlayable iid (attrs.ability 1) (UnpaidCost NoAction) (defaultWindows iid) card
        chooseOneM iid do
          labeled "Do nothing" nothing
          labeled "Discard the revealed asset to draw 1 card" do
            discardCard iid (attrs.ability 1) card
            drawCards iid (attrs.ability 1) 1
          when canPlay do
            labeled "Discard the revealed asset to draw 1 card" do
              playCardPayingCost iid card
            
      pure a
    _ -> RobertCastaigneStillHasYourBack4 <$> liftRunMessage msg attrs
