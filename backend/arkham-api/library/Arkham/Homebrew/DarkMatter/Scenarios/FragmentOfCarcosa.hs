module Arkham.Homebrew.DarkMatter.Scenarios.FragmentOfCarcosa (fragmentOfCarcosa) where

import Arkham.Helpers.Act (getCurrentActStepMaybe)
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (earnXp, scenarioI18n)
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.I18n
import Arkham.Location.Types (Field (LocationClues, LocationRevealClues))
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Projection
import Arkham.Resolution
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
  runMessage msg s@(FragmentOfCarcosa attrs) = runQueueT $ scenarioI18n "fragmentOfCarcosa" $ case msg of
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
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          actStep <- getCurrentActStepMaybe
          push $ ScenarioResolution $ Resolution $ if actStep == Just 3 then 2 else 1
        Resolution 1 -> resolution "resolution1"
        Resolution n | n `elem` [2, 3] -> earnXp attrs ("resolution" <> tshow n)
        _ -> error "invalid resolution"
      when (r /= NoResolution) endOfScenario
      pure s
    _ -> FragmentOfCarcosa <$> liftRunMessage msg attrs
