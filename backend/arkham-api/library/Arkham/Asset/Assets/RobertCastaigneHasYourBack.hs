module Arkham.Asset.Assets.RobertCastaigneHasYourBack (robertCastaigneHasYourBack) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card.Id
import Arkham.Game.Helpers (getCanPerformAbility)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (defaultWindows)

newtype RobertCastaigneHasYourBack = RobertCastaigneHasYourBack AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

robertCastaigneHasYourBack :: AssetCard RobertCastaigneHasYourBack
robertCastaigneHasYourBack = ally RobertCastaigneHasYourBack Cards.robertCastaigneHasYourBack (2, 2)

instance HasModifiersFor RobertCastaigneHasYourBack where
  getModifiersFor (RobertCastaigneHasYourBack a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities RobertCastaigneHasYourBack where
  getAbilities (RobertCastaigneHasYourBack a) =
    [ doesNotProvokeAttacksOfOpportunity
        $ restricted a 1 ControlsThis
        $ actionAbilityWithCost
        $ exhaust a
        <> HandDiscardCost
          1
          (basic (#firearm <> CardFillsLessSlots 2 #hand) <> CardWithPerformableAbility #fight [IgnoreAllCosts])
    ]

instance RunMessage RobertCastaigneHasYourBack where
  runMessage msg a@(RobertCastaigneHasYourBack attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (discardedCards -> [card]) -> do
      withCardEntity card $ handleTarget iid attrs (AssetId $ unsafeCardIdToUUID card.id)
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
    _ -> RobertCastaigneHasYourBack <$> liftRunMessage msg attrs
