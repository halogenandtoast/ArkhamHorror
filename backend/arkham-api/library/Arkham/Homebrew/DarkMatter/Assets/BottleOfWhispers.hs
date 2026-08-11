module Arkham.Homebrew.DarkMatter.Assets.BottleOfWhispers (bottleOfWhispers) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken.Types qualified as CT
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype BottleOfWhispers = BottleOfWhispers AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bottleOfWhispers :: AssetCard BottleOfWhispers
bottleOfWhispers = asset BottleOfWhispers Cards.bottleOfWhispers

{- | "Fast. / [reaction] When an investigator at your location reveals an
[auto_fail] token, remove Bottle of Whispers from the game: Cancel that token and
treat it as an [elder_sign] token instead."
-}
instance HasAbilities BottleOfWhispers where
  getAbilities (BottleOfWhispers a) =
    [ controlled a 1 ControlsThis
        $ freeReaction
        $ RevealChaosToken #after (InvestigatorAt $ locationWithAsset a.id) (ChaosTokenFaceIs CT.AutoFail)
    ]

instance RunMessage BottleOfWhispers where
  runMessage msg a@(BottleOfWhispers attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.RevealChaosToken _ token ->
          withSkillTest \sid -> do
            skillTestModifier sid (attrs.ability 1) token (ChaosTokenFaceModifier [CT.ElderSign])
            skillTestModifier sid (attrs.ability 1) token IgnoreChaosTokenEffects
        _ -> pure ()
      push $ RemoveFromGame (toTarget attrs)
      pure a
    _ -> BottleOfWhispers <$> liftRunMessage msg attrs
