module Arkham.Homebrew.DarkMatter.Scenarios.FragmentOfCarcosa (fragmentOfCarcosa) where

import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Location.Types (Field (LocationClues, LocationRevealClues))
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Projection
import Arkham.Scenario.Import.Lifted

-- Skeleton scenario for Dark Matter (homebrew). Chaos-token values, full
-- setup, and resolutions are added by later work.
newtype FragmentOfCarcosa = FragmentOfCarcosa ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fragmentOfCarcosa :: Difficulty -> FragmentOfCarcosa
fragmentOfCarcosa difficulty = scenario FragmentOfCarcosa ":dark-matter:209" "Fragment of Carcosa" difficulty []

instance HasChaosTokenValue FragmentOfCarcosa where
  getChaosTokenValue iid tokenFace (FragmentOfCarcosa attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage FragmentOfCarcosa where
  runMessage msg s@(FragmentOfCarcosa attrs) = runQueueT $ case msg of
    Setup -> runScenarioSetup FragmentOfCarcosa attrs do
      gather Set.FragmentOfCarcosa
      setAgendaDeck [Agendas.theShadowsLengthen, Agendas.shallDryAndDie]
      setActDeck [Acts.inLostCarcosa, Acts.theHeirToCarcosa, Acts.theUnspeakableTruth]
      startAt =<< place Locations.abandonedLander
    {- Guide, "Flipping Locations in Fragment": "simply flip the location, keeping
    all tokens, attachments, investigators, enemies, and other cards on that same
    location (which will have a different name). Then, add clues on that location
    up to its clue value." 'Arkham.Homebrew.DarkMatter.Helpers.flipToOtherSide'
    defers this step until after the swap has resolved. -}
    DoStep 1 (ReplaceLocation lid _ Swap) -> do
      value <- getGameValue =<< field LocationRevealClues lid
      current <- field LocationClues lid
      when (value > current) $ placeClues ScenarioSource lid (value - current)
      pure s
    ScenarioResolution _ -> do
      endOfScenario
      pure s
    _ -> FragmentOfCarcosa <$> liftRunMessage msg attrs
