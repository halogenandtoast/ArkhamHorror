module Arkham.Asset.Cards.EvanescentAscensionTheMorningStar (
  evanescentAscensionTheMorningStar,
  EvanescentAscensionTheMorningStar (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, withSkillTest)
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Modifier
import Arkham.Token

newtype EvanescentAscensionTheMorningStar = EvanescentAscensionTheMorningStar AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evanescentAscensionTheMorningStar :: AssetCard EvanescentAscensionTheMorningStar
evanescentAscensionTheMorningStar = asset EvanescentAscensionTheMorningStar Cards.evanescentAscensionTheMorningStar

instance HasAbilities EvanescentAscensionTheMorningStar where
  getAbilities (EvanescentAscensionTheMorningStar a) =
    let active = toResultDefault False a.meta
     in restrictedAbility
          a
          1
          ControlsThis
          ( ReactionAbility
              (WouldHaveSkillTestResult #when (affectsOthers $ InvestigatorAt YourLocation) #any #failure)
              (exhaust a <> assetUseCost a Wish 1)
          )
          : [ restrictedAbility a 2 (exists $ be a <> not_ (AssetWithUses Wish))
              $ SilentForcedAbility AnyWindow
            | active
            ]

instance RunMessage EvanescentAscensionTheMorningStar where
  runMessage msg a@(EvanescentAscensionTheMorningStar attrs) = runQueueT $ case msg of
    ResolvedCard _ card | toCardId card == toCardId attrs -> do
      EvanescentAscensionTheMorningStar <$> runMessage msg (setMeta True attrs)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenJustM getSkillTestInvestigator \iid' -> do
        withSkillTest \sid -> do
          tokenId <- getRandom
          x <- getChaosTokenValue iid' ElderSign iid'
          skillTestModifier sid (attrs.ability 1) sid (AddChaosTokenValue x)
          push
            $ RevealChaosToken (SkillTestSource sid) iid'
            $ ChaosToken
              { chaosTokenId = tokenId
              , chaosTokenFace = ElderSign
              , chaosTokenRevealedBy = (Just iid')
              }
          push RerunSkillTest
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      placeInBonded iid attrs
      pure a
    _ -> EvanescentAscensionTheMorningStar <$> liftRunMessage msg attrs
