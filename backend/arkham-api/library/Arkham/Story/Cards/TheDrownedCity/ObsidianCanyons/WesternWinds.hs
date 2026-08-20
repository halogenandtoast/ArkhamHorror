module Arkham.Story.Cards.TheDrownedCity.ObsidianCanyons.WesternWinds (westernWinds) where

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Direction (GridDirection (GridRight))
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.TheDrownedCity.ObsidianCanyons.Helpers
import Arkham.Story.CardDefs.TheDrownedCity.ObsidianCanyons qualified as Cards
import Arkham.Story.Import.Lifted

newtype WesternWinds = WesternWinds StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernWinds :: StoryCard WesternWinds
westernWinds = story WesternWinds Cards.westernWinds

instance HasAbilities WesternWinds where
  getAbilities (WesternWinds a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage WesternWinds where
  runMessage msg s@(WesternWinds attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Reveal tokens from the chaos bag equal to the storm intensity."
      storm <- getStormIntensity
      when (storm > 0) $ requestChaosTokens iid (attrs.ability 1) storm
      pure s
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      continue_ iid
      faces <- getModifiedChaosTokenFaces tokens
      -- "If a non-[elder sign] symbol token is revealed": any symbol token other
      -- than the elder sign lets the storm through.
      when (any (\face -> face.isSymbol && face /= ElderSign) faces) do
        -- Western Winds removes the rightmost slidable card in rows 2 and 4,
        -- then slides the remaining cards right to fill the vacated edge.
        blowWinds [2, 4] GridRight
        flipOverBy iid (attrs.ability 1) attrs
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      -- The two sides are one physical card: this face leaves play and the other
      -- takes its place, so the winds alternate direction each time they blow.
      removeStory attrs
      easternWinds <- genCard Cards.easternWinds
      push $ PlaceStory easternWinds Global
      pure s
    _ -> WesternWinds <$> liftRunMessage msg attrs
