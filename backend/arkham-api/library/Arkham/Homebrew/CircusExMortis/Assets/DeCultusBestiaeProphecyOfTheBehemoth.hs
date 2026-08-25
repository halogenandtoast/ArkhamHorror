module Arkham.Homebrew.CircusExMortis.Assets.DeCultusBestiaeProphecyOfTheBehemoth (
  deCultusBestiaeProphecyOfTheBehemoth,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Asset (assetCanHaveDamageHealed, assetCanHaveHorrorHealed)
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.DestinyAndProphecy
import Arkham.Homebrew.CircusExMortis.Helpers (sealMoonTokenOnTarget)
import Arkham.Message.Lifted.Choose

newtype DeCultusBestiaeProphecyOfTheBehemoth = DeCultusBestiaeProphecyOfTheBehemoth AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deCultusBestiaeProphecyOfTheBehemoth :: AssetCard DeCultusBestiaeProphecyOfTheBehemoth
deCultusBestiaeProphecyOfTheBehemoth =
  asset DeCultusBestiaeProphecyOfTheBehemoth Cards.deCultusBestiaeProphecyOfTheBehemoth

instance HasModifiersFor DeCultusBestiaeProphecyOfTheBehemoth where
  getModifiersFor (DeCultusBestiaeProphecyOfTheBehemoth a) =
    controllerGets a [SkillModifier #intellect 1]

instance HasAbilities DeCultusBestiaeProphecyOfTheBehemoth where
  getAbilities (DeCultusBestiaeProphecyOfTheBehemoth a) = deCultusBestiaeAbilities 0 a

instance RunMessage DeCultusBestiaeProphecyOfTheBehemoth where
  runMessage msg a@(DeCultusBestiaeProphecyOfTheBehemoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sealMoonTokenOnTarget iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      deCultusBestiaeRelease iid attrs
      doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 2) -> do
      (investigators, assets) <- healableCardsAt (attrs.ability 2) iid
      chooseOneM iid do
        targets investigators $ handleTarget iid attrs
        targets assets $ handleTarget iid attrs
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      canDamage <- canHaveDamageHealed (attrs.ability 2) iid'
      canHorror <- canHaveHorrorHealed (attrs.ability 2) iid'
      chooseHealTwo (attrs.ability 2) iid iid' canDamage canHorror
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      canDamage <- assetCanHaveDamageHealed (attrs.ability 2) aid
      canHorror <- assetCanHaveHorrorHealed (attrs.ability 2) aid
      chooseHealTwo (attrs.ability 2) iid aid canDamage canHorror
      pure a
    _ -> DeCultusBestiaeProphecyOfTheBehemoth <$> liftRunMessage msg attrs
