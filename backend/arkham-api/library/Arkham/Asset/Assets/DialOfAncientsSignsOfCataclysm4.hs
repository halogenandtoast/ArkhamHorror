module Arkham.Asset.Assets.DialOfAncientsSignsOfCataclysm4 (dialOfAncientsSignsOfCataclysm4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DialOfAncientsSignsOfCataclysm4 = DialOfAncientsSignsOfCataclysm4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dialOfAncientsSignsOfCataclysm4 :: AssetCard DialOfAncientsSignsOfCataclysm4
dialOfAncientsSignsOfCataclysm4 = asset DialOfAncientsSignsOfCataclysm4 Cards.dialOfAncientsSignsOfCataclysm4

instance HasAbilities DialOfAncientsSignsOfCataclysm4 where
  getAbilities (DialOfAncientsSignsOfCataclysm4 a) =
    [ controlled a 1 (exists $ RevealedChaosTokens #any)
        $ triggered (SkillTestResult #when (colocatedWithMatch You) AnySkillTest #failure) (exhaust a)
    , controlled a 2 criteria $ forced $ RoundEnds #when
    ]
   where
    criteria = if null a.sealedChaosTokens then Never else NoRestriction

instance RunMessage DialOfAncientsSignsOfCataclysm4 where
  runMessage msg a@(DialOfAncientsSignsOfCataclysm4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      tokens <- select $ RevealedChaosTokens #any
      focusChaosTokens tokens \unfocus -> do
        chooseTargetM iid tokens (sealChaosToken iid attrs)
        push unfocus
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      for_ attrs.sealedChaosTokens (`forTarget` msg)
      pure a
    ForTarget (ChaosTokenTarget token) (UseThisAbility iid (isSource attrs -> True) 2) -> do
      if attrs.use Charge > 0
        then do
          focusChaosTokens [token] \unfocus -> do
            chooseOneM iid do
              questionLabeled "Spend 1 charge or release this token"
              labeled "Spend 1 charge" $ spendUses (attrs.ability 2) attrs Charge 1
              targeting token $ push $ UnsealChaosToken token

            push unfocus
        else
          push $ UnsealChaosToken token
      pure a
    _ -> DialOfAncientsSignsOfCataclysm4 <$> liftRunMessage msg attrs
