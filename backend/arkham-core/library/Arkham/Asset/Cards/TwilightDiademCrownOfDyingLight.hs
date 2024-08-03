module Arkham.Asset.Cards.TwilightDiademCrownOfDyingLight (
  twilightDiademCrownOfDyingLight,
  TwilightDiademCrownOfDyingLight (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Token

newtype TwilightDiademCrownOfDyingLight = TwilightDiademCrownOfDyingLight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twilightDiademCrownOfDyingLight :: AssetCard TwilightDiademCrownOfDyingLight
twilightDiademCrownOfDyingLight = asset TwilightDiademCrownOfDyingLight Cards.twilightDiademCrownOfDyingLight

instance HasAbilities TwilightDiademCrownOfDyingLight where
  getAbilities (TwilightDiademCrownOfDyingLight x) =
    [ controlledAbility x 1 (DuringSkillTest AnySkillTest)
        $ ReactionAbility
          (RevealChaosToken #when Anyone $ oneOf [#curse, #bless])
          (assetUseCost x Offering 1 <> exhaust x)
    ]

instance RunMessage TwilightDiademCrownOfDyingLight where
  runMessage msg a@(TwilightDiademCrownOfDyingLight attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTest \sid -> do
        skillTestModifiers
          sid
          attrs
          (ChaosTokenTarget token)
          [ChaosTokenFaceModifier [#eldersign], ReturnBlessedToChaosBag, ReturnCursedToChaosBag]
      doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      when (attrs.use Offering == 0) do
        brokenDiadem <- searchBondedJust iid Cards.brokenDiademCrownOfDyingLight5
        push $ ReplaceInvestigatorAsset iid attrs.id brokenDiadem
        placeInBonded iid attrs
      pure a
    _ -> TwilightDiademCrownOfDyingLight <$> liftRunMessage msg attrs
