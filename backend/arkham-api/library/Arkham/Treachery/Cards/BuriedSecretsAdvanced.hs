module Arkham.Treachery.Cards.BuriedSecretsAdvanced (buriedSecretsAdvanced, BuriedSecretsAdvanced (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BuriedSecretsAdvanced = BuriedSecretsAdvanced TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buriedSecretsAdvanced :: TreacheryCard BuriedSecretsAdvanced
buriedSecretsAdvanced = treachery BuriedSecretsAdvanced Cards.buriedSecretsAdvanced

instance HasAbilities BuriedSecretsAdvanced where
  getAbilities (BuriedSecretsAdvanced a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [#investigate] (ActionCost 2)]

instance HasModifiersFor BuriedSecretsAdvanced where
  getModifiersFor (InvestigatorTarget iid) (BuriedSecretsAdvanced a) | a `on` iid = do
    canInvestigate <- selectAny $ locationWithInvestigator iid <> InvestigatableLocation
    pure $ toModifiers a [CannotMoveExceptByScenarioCardEffects | canInvestigate]
  getModifiersFor _ _ = pure []

instance RunMessage BuriedSecretsAdvanced where
  runMessage msg t@(BuriedSecretsAdvanced attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid (attrs.ability 1) <&> setTarget attrs
      pure t
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> BuriedSecretsAdvanced <$> liftRunMessage msg attrs
