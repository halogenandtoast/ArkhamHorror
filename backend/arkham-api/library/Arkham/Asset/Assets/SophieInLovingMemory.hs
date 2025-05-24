module Arkham.Asset.Assets.SophieInLovingMemory (sophieInLovingMemory) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype SophieInLovingMemory = SophieInLovingMemory AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sophieInLovingMemory :: AssetCard SophieInLovingMemory
sophieInLovingMemory =
  assetWith SophieInLovingMemory Cards.sophieInLovingMemory (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities SophieInLovingMemory where
  getAbilities (SophieInLovingMemory x) =
    let criteria = if x.flipped then Never else NoRestriction
     in [ controlled x 1 DuringYourSkillTest $ FastAbility $ DirectDamageCost (toSource x) You 1
        , controlled x 2 (criteria <> youExist (InvestigatorWithDamage (atLeast 5))) $ forced AnyWindow
        ]

instance RunMessage SophieInLovingMemory where
  runMessage msg a@(SophieInLovingMemory attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      let sophieItWasAllMyFault = toCard $ lookupPlayerCard Cards.sophieItWasAllMyFault attrs.cardId
      for_ attrs.controller \mark -> push $ ReplaceInvestigatorAsset mark attrs.id sophieItWasAllMyFault
      pure $ SophieInLovingMemory $ attrs & flippedL .~ True
    _ -> SophieInLovingMemory <$> liftRunMessage msg attrs
