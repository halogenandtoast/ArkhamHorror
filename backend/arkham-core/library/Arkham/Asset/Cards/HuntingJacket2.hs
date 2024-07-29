module Arkham.Asset.Cards.HuntingJacket2 (huntingJacket2, HuntingJacket2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher hiding (PlaceUnderneath)

newtype HuntingJacket2 = HuntingJacket2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingJacket2 :: AssetCard HuntingJacket2
huntingJacket2 = assetWith HuntingJacket2 Cards.huntingJacket2 ((healthL ?~ 2) . (sanityL ?~ 2))

instance HasAbilities HuntingJacket2 where
  getAbilities (HuntingJacket2 a) =
    [ controlledAbility a 1 (exists (InHandOf You <> basic NonWeakness) <> criteria1)
        $ FastAbility (exhaust a)
    , controlledAbility a 2 criteria2 $ freeReaction $ AssetDefeated #when ByAny (be a)
    ]
   where
    criteria1 = if length a.cardsUnderneath >= 3 then Never else NoRestriction
    criteria2 = if length a.cardsUnderneath > 0 then NoRestriction else Never

instance RunMessage HuntingJacket2 where
  runMessage msg a@(HuntingJacket2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOneToHandle iid (attrs.ability 1) $ inHandOf iid <> basic NonWeakness
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      c <- getCard cid
      obtainCard c
      push $ PlaceUnderneath (toTarget attrs) [c]
      gainResourcesIfCan iid (attrs.ability 1) (1 + length attrs.cardsUnderneath)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      for_ attrs.cardsUnderneath \case
        PlayerCard pc -> push $ InvestigatorDrewPlayerCard iid pc
        EncounterCard ec -> push $ InvestigatorDrewEncounterCard iid ec
        _ -> error "impossible"
      pure a
    _ -> HuntingJacket2 <$> liftRunMessage msg attrs
