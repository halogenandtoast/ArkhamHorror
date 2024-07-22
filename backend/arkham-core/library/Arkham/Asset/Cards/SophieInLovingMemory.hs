module Arkham.Asset.Cards.SophieInLovingMemory (sophieInLovingMemory, SophieInLovingMemory (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Matcher
import Arkham.Prelude

newtype SophieInLovingMemory = SophieInLovingMemory AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sophieInLovingMemory :: AssetCard SophieInLovingMemory
sophieInLovingMemory =
  assetWith
    SophieInLovingMemory
    Cards.sophieInLovingMemory
    (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities SophieInLovingMemory where
  getAbilities (SophieInLovingMemory x) =
    let flippedCriteria = if x.flipped then Never else NoRestriction
     in [ controlledAbility x 1 (DuringSkillTest (YourSkillTest AnySkillTest))
            $ FastAbility
            $ DirectDamageCost (toSource x) You 1
        , controlledAbility x 2 (flippedCriteria <> youExist (InvestigatorWithDamage (atLeast 5)))
            $ forced AnyWindow
        ]

instance RunMessage SophieInLovingMemory where
  runMessage msg a@(SophieInLovingMemory attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Flip iid (attrs.ability 2) (toTarget attrs)
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      let
        sophieItWasAllMyFault =
          PlayerCard
            $ lookupPlayerCard Cards.sophieItWasAllMyFault (toCardId attrs)
        markId = getController attrs
      push $ ReplaceInvestigatorAsset markId attrs.id sophieItWasAllMyFault
      pure $ SophieInLovingMemory $ attrs & flippedL .~ True
    _ -> SophieInLovingMemory <$> runMessage msg attrs
