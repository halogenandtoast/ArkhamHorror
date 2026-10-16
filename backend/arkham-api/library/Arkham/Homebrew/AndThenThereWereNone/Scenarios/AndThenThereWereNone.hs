{- | And Then There Were None -- a fan side story.

Ten guests came to Soldier Island and ten corpses were waiting on Monday
morning. The scenario is a whodunnit played with skill icons: ten [[Victim]]
story assets stand two to a room, ten [[Method]] cards lie face down beneath
those same rooms, and act 1 is spent buying the methods face up and pinning
them to the guests they killed. At the end of it the investigators name a
killer; act 1b checks their answer against the icon tally and the convicted
guest rises as the [[Culprit]].

Setup's pile ritual -- "separate the Method cards by skill icons and make five
piles so that you do not know which is which", then thin two piles by two and a
third by one -- is carried out here at random. The piles are indistinguishable
by construction, so whatever the players choose induces exactly the uniform
distribution a shuffle does; nothing is learned either way.

Every location prints its own symbol and connections, so the grid below is
layout only. Standalone has two difficulties, Standard and Hard, chosen off
'isEasyStandard'.
-}
module Arkham.Homebrew.AndThenThereWereNone.Scenarios.AndThenThereWereNone (
  andThenThereWereNone,
) where

import Arkham.Card (CardDef)
import Arkham.Card.CardDef (cdSkills)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Homebrew.AndThenThereWereNone.Key
import Arkham.Homebrew.AndThenThereWereNone.Sets qualified as Set
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Types (Field (LocationCardsUnderneath))
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.SkillType (SkillIcon (..), SkillType (..))

newtype AndThenThereWereNone = AndThenThereWereNone ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

andThenThereWereNone :: Difficulty -> AndThenThereWereNone
andThenThereWereNone difficulty =
  sideStory
    AndThenThereWereNone
    ":and-then-there-were-none:001"
    "And Then There Were None"
    difficulty
    [ ".          bedrooms ."
    , "diningRoom .        sunroom"
    , ".          terrace  ."
    , "woodShed   .        beach"
    ]

{- | The five non-[[Central]] rooms, paired with their grid label.

Two [[Victim]] assets and two facedown [[Method]] cards go to each of them, so
this list is what both halves of the deal are zipped against.
-}
outerLocations :: [(Text, CardDef)]
outerLocations =
  [ ("bedrooms", Locations.bedrooms)
  , ("diningRoom", Locations.diningRoom)
  , ("sunroom", Locations.sunroom)
  , ("woodShed", Locations.woodShed)
  , ("beach", Locations.beach)
  ]

{- | The fifteen [[Method]] cards "separated by skill icons" -- five piles of
three, in icon order, which is the order setup then destroys by shuffling.
-}
methodPiles :: [[CardDef]]
methodPiles = [[d | d <- methodDefs, cdSkills d == [icon]] | icon <- allMethodIcons]
 where
  allMethodIcons =
    [ SkillIcon SkillWillpower
    , SkillIcon SkillIntellect
    , SkillIcon SkillCombat
    , SkillIcon SkillAgility
    , WildIcon
    ]

instance HasChaosTokenValue AndThenThereWereNone where
  getChaosTokenValue iid chaosTokenFace (AndThenThereWereNone attrs) = case chaosTokenFace of
    -- "-X. X is the number of locations without encounter cards beneath them
    -- (max -4)." Hard prints no maximum.
    Skull -> do
      n <- countM (fieldMap LocationCardsUnderneath null) =<< select Anywhere
      pure $ toChaosTokenValue attrs Skull (min 4 n) n
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    -- "-4 instead if this is a test against a _Geist_ enemy" ("-6 instead" on Hard).
    ElderThing -> do
      geist <- isTestAgainstGeist
      pure
        $ if geist
          then toChaosTokenValue attrs ElderThing 4 6
          else toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage AndThenThereWereNone where
  runMessage msg s@(AndThenThereWereNone attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      scope "intro" $ flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup AndThenThereWereNone attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "victims"
        li "methods"
        li "setAside"
        unscoped $ li "shuffleRemainder"

      gather Set.AndThenThereWereNone

      -- Every connection is printed, so nothing is wired here.
      startAt =<< placeLabeled "terrace" Locations.terrace
      outer <- for outerLocations (uncurry placeLabeled)

      -- "Randomly put two Victim story assets into play at each non-Central location."
      dealt <- shuffleM victimDefs
      for_ (zip (concatMap (replicate 2) outer) dealt) \(lid, guest) -> assetAt_ guest lid

      {- "Choose two piles and remove two cards in each pile from the game. Choose
      one of the other three piles and remove one card in that pile from the game.
      Shuffle the remaining Method cards together and place two facedown beneath
      each non-Central location."

      All fifteen are set aside first so none of them is left in the encounter
      deck, and the five that are removed are taken out of the set-aside zone as
      well -- leaving them there face up would show the players exactly which
      icons were thinned, which is the one thing setup is protecting. -}
      setAside methodDefs
      piles <- shuffleM methodPiles
      let (thinnedByTwo, rest) = splitAt 2 piles
      let (thinnedByOne, untouched) = splitAt 1 rest
      let kept = concat $ map (drop 2) thinnedByTwo <> map (drop 1) thinnedByOne <> untouched
      let removed = concat $ map (take 2) thinnedByTwo <> map (take 1) thinnedByOne
      traverse_ (removeCardFromGame <=< fromSetAside) removed
      methods <- shuffleM =<< traverse fromSetAside kept
      for_ (zip (concatMap (replicate 2) outer) methods) \(lid, card) ->
        placeUnderneath lid [card]

      setAside $ Assets.spectralConduit : replicate 4 Assets.antediluvianSigils

      setAgendaDeck [Agendas.soldieringOn]
      setActDeck [Acts.causeUNOwen, Acts.bestLaidPlans]
    -- "If you control more than 1[per_investigator] clues, take 1 horror."
    ResolveChaosToken _ Cultist iid -> do
      threshold <- perPlayer 1
      clues <- field InvestigatorClues iid
      when (clues > threshold) $ assignHorror iid Cultist 1
      pure s
    {- "If there is an icon on a Method card at your location that matches the skill
    for this test, take 1 damage." -}
    ResolveChaosToken _ Tablet iid -> do
      withLocationOf iid \lid ->
        whenM (methodIconMatchesTestAt lid) $ assignDamage iid Tablet 1
      pure s
    ScenarioResolution r -> do
      {- "Each investigator gains 1 bonus experience for every 3 Method cards
      attached to Victim cards at the end of the scenario." The convicted guest is
      a [[Culprit]] enemy by now, so anything riding them stopped counting.

      Scored outside @scope "resolutions"@ so the bonus reads its label from the
      scenario's own @xp@ scope, where every other scenario keeps it. -}
      solved <- selectCount $ methodAsset <> AssetAttachedToAsset victimAsset
      let bonus = toBonus "bonus" (solved `div` 3)
      scope "resolutions" $ case r of
        NoResolution -> push $ ScenarioResolution $ Resolution 1
        Resolution 1 -> do
          record SoldierIslandWasAbandoned
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs bonus
          endOfScenario
        Resolution 2 -> do
          record TheSoldierIslandMurdersWereSolved
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs bonus
          -- "An investigator may choose to add Spectral Conduit to their deck. This
          -- card does not count toward that investigator's deck size."
          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.spectralConduit
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> AndThenThereWereNone <$> liftRunMessage msg attrs

-- | Standalone offers Standard and Hard only.
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Hard -> hardBag
  Expert -> hardBag
  _ -> standardBag
 where
  symbols = [Skull, Skull, Cultist, Tablet, ElderThing, AutoFail, ElderSign]
  standardBag =
    [PlusOne, Zero, MinusOne, MinusOne, MinusTwo, MinusThree, MinusThree, MinusFour] <> symbols
  hardBag =
    [Zero, MinusOne, MinusTwo, MinusThree, MinusFour, MinusFour, MinusFive, MinusSix] <> symbols
