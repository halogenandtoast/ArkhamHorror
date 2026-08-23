module Arkham.Asset.Assets.ForgedPermit (forgedPermit) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Cost qualified as Cost
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype ForgedPermit = ForgedPermit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forgedPermit :: AssetCard ForgedPermit
forgedPermit = asset ForgedPermit Cards.forgedPermit

instance HasAbilities ForgedPermit where
  getAbilities (ForgedPermit a) = [restricted a 1 ControlsThis $ FastAbility (discardCost a)]

instance RunMessage ForgedPermit where
  runMessage msg a@(ForgedPermit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <-
        select $ PlayableCard Cost.PaidCost $ inHandOf ForPlay iid <> basic (#asset <> #item)
      chooseTargetM iid cards \card -> do
        push $ PutCardIntoPlay iid card (Just $ toTarget attrs) NoPayment (defaultWindows iid)
        push $ HandleTargetChoice iid (attrs.ability 1) (CardIdTarget card.id)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      selectOne (AssetWithCardId cid) >>= traverse_ \aid -> do
        abilities <-
          map (`applyAbilityModifiers` [IgnoreActionCost])
            <$> select
              ( AbilityIsActionAbility
                  <> AbilityOnAsset (AssetWithId aid)
                  <> PerformableAbilityBy (InvestigatorWithId iid) [IgnoreActionCost]
              )
        unless (null abilities) $ chooseOneM iid $ withI18n do
          for_ abilities \ab -> abilityLabeled iid ab nothing
          unscoped skip_
      pure a
    _ -> ForgedPermit <$> liftRunMessage msg attrs
