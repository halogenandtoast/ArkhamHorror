module Arkham.Homebrew.DarkMatter.Locations.ThresholdOfYuggoth (thresholdOfYuggoth) where

import Arkham.Ability
import Arkham.ChaosToken.Types qualified as CT
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (cancelPendingScan, wouldScanEventAt)
import Arkham.Location.Import.Lifted hiding (PerformAction)
import Arkham.Matcher

newtype ThresholdOfYuggoth = ThresholdOfYuggoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thresholdOfYuggoth :: LocationCard ThresholdOfYuggoth
thresholdOfYuggoth =
  symbolLabel $ location ThresholdOfYuggoth Cards.thresholdOfYuggoth 3 (PerPlayer 2)

{- | "Forced - When you would scan at Threshold of Yuggoth: Reveal a random chaos
token for each clue on Threshold of Yuggoth. If you reveal a [skull], [cultist],
[tablet], [elder_thing] or [auto_fail] token, cancel that scan and take 1 horror
instead."

Gated on the scan's anchor rather than 'Here': a remote "scan as if you were
at that location" (Universal Archives) must still trigger this, and an
ordinary scan performed while merely standing here for somewhere else must
not.
-}
instance HasAbilities ThresholdOfYuggoth where
  getAbilities (ThresholdOfYuggoth a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ CampaignEvent #when (Just You) (wouldScanEventAt a.id)

badFaces :: [CT.ChaosTokenFace]
badFaces = [CT.Skull, CT.Cultist, CT.Tablet, CT.ElderThing, CT.AutoFail]

instance RunMessage ThresholdOfYuggoth where
  runMessage msg l@(ThresholdOfYuggoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      when (attrs.clues > 0) $ requestChaosTokens iid (attrs.ability 1) attrs.clues
      pure l
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      continue_ iid
      when (any ((`elem` badFaces) . CT.chaosTokenFace) tokens) do
        cancelPendingScan
        assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> ThresholdOfYuggoth <$> liftRunMessage msg attrs
