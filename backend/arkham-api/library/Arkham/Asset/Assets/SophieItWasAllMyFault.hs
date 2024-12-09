module Arkham.Asset.Assets.SophieItWasAllMyFault (sophieItWasAllMyFault, SophieItWasAllMyFault (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Matcher
import Arkham.Prelude

newtype SophieItWasAllMyFault = SophieItWasAllMyFault AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sophieItWasAllMyFault :: AssetCard SophieItWasAllMyFault
sophieItWasAllMyFault =
  assetWith
    SophieItWasAllMyFault
    Cards.sophieItWasAllMyFault
    (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities SophieItWasAllMyFault where
  getAbilities (SophieItWasAllMyFault x) =
    let flippedCriteria = if x.flipped then Never else NoRestriction
     in [ controlledAbility x 2 (flippedCriteria <> youExist (InvestigatorWithDamage (atMost 4)))
            $ forced AnyWindow
        ]

instance HasModifiersFor SophieItWasAllMyFault where
  getModifiersFor (SophieItWasAllMyFault attrs) = controllerGets attrs [AnySkillValue (-1)]

instance RunMessage SophieItWasAllMyFault where
  runMessage msg a@(SophieItWasAllMyFault attrs) = case msg of
    -- This is a hack because Sophie can flip while paying for her first
    -- ability causing her to flip before this ability is resolved. If we use
    -- the normal indexing then this will actually flip back over
    -- unintentionally and required triggering the forced ability to flip her
    -- back over again.
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        pushM $ skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Flip iid (toSource attrs) (toTarget attrs)
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      let
        sophieInLovingMemory =
          PlayerCard
            $ lookupPlayerCard Cards.sophieInLovingMemory (toCardId attrs)
        markId = getController attrs
      push $ ReplaceInvestigatorAsset markId attrs.id sophieInLovingMemory
      pure $ SophieItWasAllMyFault $ attrs & flippedL .~ True
    _ -> SophieItWasAllMyFault <$> runMessage msg attrs
