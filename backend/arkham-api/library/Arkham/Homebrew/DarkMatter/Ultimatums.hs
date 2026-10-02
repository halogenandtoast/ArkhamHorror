{- | Dark Matter's own ultimatums (campaign guide), hooked from the campaign's
runMessage beside the achievements.

Two of the seven are not here. The Ultimatum of Exploration rewrites scanning
itself, so it lives in "Arkham.Homebrew.DarkMatter.Helpers" where the scan is
run; the Ultimatum of Anachronism is a modifier, so it lives in the campaign's
'HasModifiersFor'. The Ultimatum of the Unspeakable Oath is an honor rule with a
button in the scenario UI and no engine behavior at all.

Scenario setup is reached through 'EndSetup' rather than 'Setup': the campaign
sees a message before the scenario does, so at 'Setup' the scenario's builder
body has not run yet and neither the scanning deck nor the agenda exists.
-}
module Arkham.Homebrew.DarkMatter.Ultimatums (
  runDarkMatterUltimatums,
) where

import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Deck qualified as Deck
import Arkham.Helpers.ChaosBag (getBagChaosTokens)
import Arkham.Helpers.EncounterSet (gatherEncounterSet)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.DarkMatter.Helpers
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.UltimatumDefs
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Prelude

runDarkMatterUltimatums :: ReverseQueue m => Message -> m ()
runDarkMatterUltimatums = \case
  -- "Begin the campaign with 3 tally marks under Impending Doom."
  CampaignStep PrologueStep ->
    whenM (hasDarkMatterUltimatum UltimatumOfInevitability) $ addImpendingDoom 3
  {- The Dark Past set has to be in the encounter deck before the scenario
  shuffles it, and the campaign sees 'Setup' first, so this one is the exception
  that does belong there. -}
  Setup -> whenM (hasDarkMatterUltimatum UltimatumOfTheDarkPast) do
    deck <- getEncounterDeck
    unless (any (`cardMatch` CardFromEncounterSet Set.DarkPast) (toList deck)) do
      cards <- map toCard <$> gatherEncounterSet Set.DarkPast
      shuffleCardsIntoDeck Deck.EncounterDeck cards
  EndSetup -> do
    -- "Begin each scenario with doom on the agenda equal to Impending Doom."
    whenM (hasDarkMatterUltimatum UltimatumOfImpendingDoom) do
      placeDoomOnAgenda =<< getImpendingDoom

    {- "During the setup of each scenario with a scanning deck, shuffle The
    Feaster from Afar into the scanning deck." Minted rather than taken from the
    set-aside pile: The Tatterdemalion has a scanning deck but never sets the
    Feaster aside. -}
    whenM (hasDarkMatterUltimatum UltimatumOfTheFeaster) do
      whenM (notNull <$> getScanningDeck) do
        feaster <- genCard Enemies.theFeasterFromAfar
        shuffleIntoScanningDeck [feaster]

  {- "During the resolution of those scenarios, if there is a copy of
  Reminiscence in the victory display, add a [tablet] token to the chaos bag (if
  there are already 4, each investigator suffers 1 mental trauma instead)."
  'EndOfGame' is the resolution hook: the victory display and the bag are both
  still live, and a resolution cannot be keyed on directly. -}
  EndOfGame _ -> whenM (hasDarkMatterUltimatum UltimatumOfTheDarkPast) do
    whenM hasReminiscenceInVictoryDisplay do
      tablets <- count ((== Tablet) . (.face)) <$> getBagChaosTokens
      if tablets >= 4
        then eachInvestigator (`sufferMentalTrauma` 1)
        else addChaosToken Tablet
  _ -> pure ()

-- | Shared by 'addReminiscenceToken' and the Ultimatum of the Dark Past.
hasReminiscenceInVictoryDisplay :: HasGame m => m Bool
hasReminiscenceInVictoryDisplay =
  selectAny
    $ VictoryDisplayCardMatch
    $ basic
    $ mapOneOf
      cardIs
      [ Treacheries.reminiscencePledge
      , Treacheries.reminiscenceSecrets
      , Treacheries.reminiscenceCovenant
      ]
