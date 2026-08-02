module Arkham.Story.Cards.EasternWinds (easternWinds) where

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Direction (GridDirection (GridLeft))
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype EasternWinds = EasternWinds StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easternWinds :: StoryCard EasternWinds
easternWinds = story EasternWinds Cards.easternWinds

instance HasAbilities EasternWinds where
  getAbilities (EasternWinds a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage EasternWinds where
  runMessage msg s@(EasternWinds attrs) = runQueueT $ case msg of
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
        -- Eastern Winds removes the leftmost slidable card in rows 1 and 3,
        -- then slides the remaining cards left to fill the vacated edge.
        blowWinds [1, 3] GridLeft
        flipOverBy iid (attrs.ability 1) attrs
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      -- The two sides are one physical card: this face leaves play and the other
      -- takes its place, so the winds alternate direction each time they blow.
      removeStory attrs
      westernWinds <- genCard Cards.westernWinds
      push $ PlaceStory westernWinds Global
      pure s
    _ -> EasternWinds <$> liftRunMessage msg attrs
