module Arkham.Act.Cards.CarefulNavigation (carefulNavigation) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Vault))

newtype CarefulNavigation = CarefulNavigation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carefulNavigation :: ActCard CarefulNavigation
carefulNavigation = act (1, A) CarefulNavigation Cards.carefulNavigation Nothing

instance HasAbilities CarefulNavigation where
  getAbilities (CarefulNavigation a) =
    extend
      a
      [ -- [reaction] After the investigators have translated any 3 of the
        -- following glyphs (rune_l, rune_m, rune_n, rune_p, rune_o): Reveal the
        -- Sealed Chamber location.
        --
        -- TODO: the "any 3 of these 5 glyphs" threshold cannot be expressed
        -- cleanly: glyph translation only pushes a boolean record
        -- (TheInvestigatorsDiscoveredAnAlienLanguage) plus an unread
        -- `campaignSpecific "translateGlyph"` payload, so there is no readable
        -- per-glyph count to gate this on. Best-effort: trigger off the
        -- "translateGlyph" campaign event (the semantically correct window),
        -- restricted so it only fires while the Sealed Chamber is not yet in
        -- play. NOTE: for this to actually fire, the glyph locations must
        -- `checkAfter (Window.CampaignEvent ... "translateGlyph")` when they
        -- translate a glyph (they currently only push the record + payload).
        restricted a 1 (notExists $ locationIs Locations.chamberOfTheTabletUnsealed)
          $ freeReaction (CampaignEvent #after Nothing "translateGlyph")
      , -- (Objective) When the round ends, if an investigator controls the Tidal
        -- Tablet story asset, advance.
        restricted a 2 (exists $ assetIs Assets.tidalTablet <> AssetControlledBy Anyone)
          $ Objective
          $ forced (RoundEnds #when)
      ]

instance RunMessage CarefulNavigation where
  runMessage msg a@(CarefulNavigation attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- "Sealed Chamber" is the front side of the Chamber of the Tablet; only the
      -- (set-aside) chamberOfTheTabletUnsealed location def is registered, so we
      -- place that and reveal it. TODO: if the Sealed Chamber side is later given
      -- its own location def, place/reveal that side here instead.
      sealedChamber <- placeSetAsideLocation Locations.chamberOfTheTabletUnsealed
      reveal sealedChamber
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide A attrs -> True) _ _ -> do
      -- Increase the flood level of each location that can have its flood level
      -- increased.
      selectEach CanHaveFloodLevelIncreased (push . IncreaseFloodLevel)
      -- Place clues on each revealed Vault location without victory, up to its
      -- clue threshold.
      selectEach (RevealedLocation <> LocationWithTrait Vault <> not_ LocationWithVictory)
        $ placeCluesUpToClueValue attrs
      advanceActDeck attrs
      pure a
    _ -> CarefulNavigation <$> liftRunMessage msg attrs
