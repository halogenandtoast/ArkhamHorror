module Arkham.Homebrew.DarkMatter.Assets.BottleOfWhispers (bottleOfWhispers) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken.Types qualified as CT
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Window (getRevealedChaosTokens)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher

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
    -- "treat it as an [elder_sign] token instead" leaves the elder sign's own
    -- effect to resolve, so the face is swapped without ignoring its effects
    -- (same shape as Eucatastrophe (3), which prints the identical clause)
    UseCardAbility _ (isSource attrs -> True) 1 (getRevealedChaosTokens -> tokens) _ -> do
      for_ tokens \token ->
        chaosTokenEffect (attrs.ability 1) token (ChaosTokenFaceModifier [CT.ElderSign])
      push $ RemoveFromGame (toTarget attrs)
      pure a
    _ -> BottleOfWhispers <$> liftRunMessage msg attrs
