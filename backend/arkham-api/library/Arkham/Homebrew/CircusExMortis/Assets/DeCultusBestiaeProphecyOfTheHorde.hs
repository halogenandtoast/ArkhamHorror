module Arkham.Homebrew.CircusExMortis.Assets.DeCultusBestiaeProphecyOfTheHorde (
  deCultusBestiaeProphecyOfTheHorde,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Healing
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy
import Arkham.Homebrew.CircusExMortis.Helpers (sealMoonTokenOnTarget)
import Arkham.Message.Lifted.Choose

newtype DeCultusBestiaeProphecyOfTheHorde = DeCultusBestiaeProphecyOfTheHorde AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deCultusBestiaeProphecyOfTheHorde :: AssetCard DeCultusBestiaeProphecyOfTheHorde
deCultusBestiaeProphecyOfTheHorde =
  asset DeCultusBestiaeProphecyOfTheHorde Cards.deCultusBestiaeProphecyOfTheHorde

instance HasModifiersFor DeCultusBestiaeProphecyOfTheHorde where
  getModifiersFor (DeCultusBestiaeProphecyOfTheHorde a) =
    controllerGets a [SkillModifier #intellect 1]

instance HasAbilities DeCultusBestiaeProphecyOfTheHorde where
  getAbilities (DeCultusBestiaeProphecyOfTheHorde a) = deCultusBestiaeAbilities 0 a

instance RunMessage DeCultusBestiaeProphecyOfTheHorde where
  runMessage msg a@(DeCultusBestiaeProphecyOfTheHorde attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sealMoonTokenOnTarget iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      deCultusBestiaeRelease iid attrs
      doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      (investigators, assets) <- healableCardsAt (attrs.ability 2) iid
      chooseUpToNM_ iid 2 do
        targets investigators $ handleTarget iid attrs
        targets assets $ handleTarget iid attrs
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      chooseHealDamageOrHorrorOn (attrs.ability 2) iid iid'
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      assetChooseHealDamageOrHorror (attrs.ability 2) iid aid
      pure a
    _ -> DeCultusBestiaeProphecyOfTheHorde <$> liftRunMessage msg attrs
