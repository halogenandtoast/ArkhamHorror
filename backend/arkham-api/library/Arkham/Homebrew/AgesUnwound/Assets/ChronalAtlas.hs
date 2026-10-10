module Arkham.Homebrew.AgesUnwound.Assets.ChronalAtlas (chronalAtlas) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosBagStepState
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.Window (getDrawSource)
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype ChronalAtlas = ChronalAtlas AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Uses (3 secrets)."

TODO(ages-unwound): this belongs on the def as @cdUses = Uses Secret (Static 3)@,
but @CardDefs/Assets.hs@ is the orchestrator's. Setting 'printedUsesL' in the
builder is the same thing at runtime -- uses are seeded from
@assetPrintedUses@ when the asset enters play -- and is what Jenny's Twin .45s
already does.
-}
chronalAtlas :: AssetCard ChronalAtlas
chronalAtlas = assetWith ChronalAtlas Cards.chronalAtlas (printedUsesL .~ Uses Secret (Fixed 3))

-- | "You get +1 [intellect]."
instance HasModifiersFor ChronalAtlas where
  getModifiersFor (ChronalAtlas a) = controllerGets a [SkillModifier #intellect 1]

{- | "[reaction] When you would reveal a chaos token, exhaust Chronal Atlas and
spend 1 secret: Reveal 2 chaos tokens instead of 1. Choose 1 of those tokens to
resolve, and seal the other here. /
__Forced__ - At the end of the round: Return any tokens sealed on Chronal Atlas
to the chaos bag."

The Forced is gated on there being something to return; with nothing sealed it
would otherwise prompt every round for no effect.
-}
instance HasAbilities ChronalAtlas where
  getAbilities (ChronalAtlas a) =
    [ restricted a 1 ControlsThis
        $ triggered (WouldRevealChaosToken #when You) (exhaust a <> assetUseCost a Secret 1)
    , restricted a 2 (if null a.sealedChaosTokens then Never else NoRestriction)
        $ forced
        $ RoundEnds #when
    ]

{- | The two-token draw is Grotesque Statue's; the difference is where the
unchosen token goes. @ResolveChoice@ announces it as @ChaosTokenIgnored@, which
is the only hook that names it, and sealing it there removes it from the
set-aside pile before the end of the test would return it to the bag.
-}
instance RunMessage ChronalAtlas where
  runMessage msg a@(ChronalAtlas attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawSource -> drawSource) _ -> do
      checkWhen $ Window.WouldRevealChaosTokens drawSource iid
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose (toSource attrs) 1 ResolveChoice [Undecided Draw, Undecided Draw] [] Nothing
      pure a
    ChaosTokenIgnored iid (isSource attrs -> True) token -> do
      sealChaosToken iid attrs token
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      for_ attrs.sealedChaosTokens unsealChaosToken
      pure a
    _ -> ChronalAtlas <$> liftRunMessage msg attrs
