module Arkham.Homebrew.CircusExMortis.Scenarios.ThousandToOne (thousandToOne) where

import Arkham.Card
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers (unDeck)
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Id (InvestigatorId, LocationId)
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Layout (GridTemplateRow)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Story (resolveStory)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype ThousandToOne = ThousandToOne ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thousandToOne :: Difficulty -> ThousandToOne
thousandToOne difficulty =
  scenario
    ThousandToOne
    ":circus-ex-mortis:190"
    "Thousand to One"
    difficulty
    thousandToOneLayout

{- | A single vertical chain, four columns wide: Primal Forest at the top (where
Shub-Niggurath starts) down through High Thicket and Sparse Woodland to Silent Clearing,
with Mossy Glen and Fallen Copse side by side below it. Every connection comes from the
printed symbols on the location cards; this only says where each card is drawn.

The four Destiny locations, which only arrive when a destiny is resolved, sit in a 2x2
block to the right of Silent Clearing rather than in a row of their own below the chain.
They need a named cell each: without one the grid auto-places them into an empty @.@ slot
flanking the chain, which reads as a location sitting beside Primal Forest.

The block's arrangement is what keeps the lines short. Each of them connects to Silent
Clearing (Moon) and to both of the two carrying the opposite symbol -- Forest Chasm and
Marked Grove are Star, Canyon Entrance and Defiled Woods are Plus -- so the four
Star-to-Plus connections are a complete crossing. Putting the two Stars on one diagonal
and the two Pluses on the other makes every one of those four a neighbour, horizontally
or vertically. The gutter column keeps the block clear of the chain, and sitting it on
Silent Clearing's own row makes two of its four links horizontal.
-}
thousandToOneLayout :: [GridTemplateRow]
thousandToOneLayout =
  [ ". primalForest primalForest . . . . ."
  , ". highThicket highThicket . . . . ."
  , ". sparseWoodland sparseWoodland . forestChasm forestChasm canyonEntrance canyonEntrance"
  , ". silentClearing silentClearing . defiledWoods defiledWoods markedGrove markedGrove"
  , "mossyGlen mossyGlen fallenCopse fallenCopse . . . ."
  ]

{- | The eight destiny words the Fata Diana deals in "Written in Stone", each with the
Destiny story card that reads it out. Setup resolves the ones recorded in the campaign log
and removes the rest from the game; each story's own module knows where its back belongs.
-}
destinyStories :: [(Text, CardDef)]
destinyStories =
  [ ("heart", Stories.strikeTheHeart)
  , ("pipes", Stories.silenceThePipes)
  , ("torch", Stories.raiseTheTorch)
  , ("rock", Stories.splitTheRock)
  , ("sigil", Stories.scribeTheSigil)
  , ("stain", Stories.cleanseTheStain)
  , ("prayer", Stories.reciteThePrayer)
  , ("burden", Stories.bearTheBurden)
  ]

{- | Republishes each investigator's destiny as a 'ScenarioModifier' so cards can name it
from a pure 'getAbilities' -- the same seam Bacchanalia uses for its vices. The log itself
stays the source of truth; 'Arkham.Homebrew.CircusExMortis.Helpers.hasDestiny' is what
reads it from inside a card's 'HasModifiersFor'.
-}
instance HasModifiersFor ThousandToOne where
  getModifiersFor (ThousandToOne a) = modifySelectMapM a Anyone \iid ->
    maybeToList . fmap (ScenarioModifier . destinyKey) <$> destinyOf iid

{- | "X is the number of ☾ tokens sealed on investigator cards at your location"
(Hard/Expert: 1 + that number). Read off every investigator standing with you, your own
card included -- 'colocatedWith' covers both. Investigator cards only: a ☾ sealed on an
asset they control is not sealed on an investigator card and does not count.
-}
getSealedMoonTokensAt :: HasGame m => InvestigatorId -> m Int
getSealedMoonTokensAt iid = do
  iids <- select $ colocatedWith iid
  sum <$> traverse (fmap length . getSealedMoonTokens) iids

instance HasChaosTokenValue ThousandToOne where
  getChaosTokenValue iid tokenFace (ThousandToOne attrs) = case tokenFace of
    Skull -> do
      n <- getSealedMoonTokensAt iid
      let extra = if isHardExpert attrs then 1 else 0
      pure $ ChaosTokenValue Skull (NegativeModifier (extra + n))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    MoonToken -> pure moonTokenValue
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | The ☾ riders on the cultist and tablet tokens: "if a ☾ token is revealed during this
test, choose and discard a card from your hand / lose a resource".

Whether a ☾ was revealed is only settled once every token of the test is out, so these ride
the per-token 'PassedSkillTest' / 'FailedSkillTest' the engine raises for each revealed
token at ST.7 rather than the token's own 'ResolveChaosToken'. A ☾ drawn after the cultist
still counts, which is what the printed "during this test" asks for.
-}
moonTokenRider
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> ChaosTokenFace -> m ()
moonTokenRider source iid face = when (face `elem` [Cultist, Tablet]) do
  revealed <- getModifiedChaosTokenFaces =<< getSkillTestRevealedChaosTokens
  when (MoonToken `elem` revealed) case face of
    Cultist -> chooseAndDiscardCard iid source
    _ -> loseResources iid source 1

{- | The investigator holding a named card in their deck, and the copy itself. Both cards
the intro asks about are unique, so at most one seat can answer.

Matched by title rather than by 'CardDef': All Points West swaps both Lady Esprit and
Monstrous Transformation for their Circus printings, so which def sits in the deck depends
on the route the campaign took.
-}
deckCardHolder :: HasGame m => Text -> m (Maybe (InvestigatorId, Card))
deckCardHolder title = do
  iids <- select $ DeckWith $ HasCard $ CardWithTitle title
  holders <- for iids \iid -> do
    deck <- field InvestigatorDeck iid
    pure $ (\c -> (iid, toCard c)) <$> find ((`cardMatch` CardWithTitle title) . toCard) (unDeck deck)
  pure $ listToMaybe (catMaybes holders)

{- | "You may begin the game with <card> in your hand as an additional card." Reserved here
rather than at @Setup@: opening hands and mulligans are dealt before setup runs, so
'AdditionalStartingCards' has to already be in place -- the same reservation Harm's Way makes
for Amalthea Weaver and De Cultus Bestiae.

One label pair is shared by both cards, as in Harm's Way: the green entry just above has
already named which card is on offer, and 'focusCards' puts it on screen beside the ask.
-}
beginWithCardInHand :: (HasI18n, ReverseQueue m) => InvestigatorId -> Card -> m ()
beginWithCardInHand iid card = focusCards [card] $ scope "startingCards" $ chooseOneM iid do
  questionLabeledCard iid
  labeled "take" do
    push $ ObtainCard (toCardId card)
    setupModifier ScenarioSource iid (AdditionalStartingCards [card])
  labeled "leave" nothing

{- | "Put one copy of Ravenous Brood into play at High Thicket, either side face up." The
two faces are separate defs with separate card codes, so the chosen face has to be written
back to the card map before the entity is built, or the engine comes up on whichever face
the card was generated on.

ponytail: the same two lines as @spawnBroodAt@ in @Enemies.ShubNiggurath@, which does the
Forced spawn from the set-aside pile; lift both into Helpers if a third caller appears.
-}
putBroodIntoPlay :: ReverseQueue m => Card -> LocationId -> m ()
putBroodIntoPlay brood lid = do
  push $ ReplaceCard brood.id brood
  createEnemyAt_ brood lid

instance RunMessage ThousandToOne where
  runMessage msg s@(ThousandToOne attrs) = runQueueT $ scenarioI18n "thousandToOne" $ case msg of
    {- The intro is one story that accumulates. Both conditional lines are always rendered,
    validated so the table can see which branch applies (All Points West's "What a Horrible
    Night" / "Good Juju" pair). A page only ends -- and so is only shown on its own -- when
    somebody actually holds the card and there is a question to ask them; otherwise its
    content carries forward and is read as one entry with the page that follows. With neither
    card in play the whole intro is a single entry ending in the choose-one. -}
    PreScenarioSetup -> scope "intro" do
      monstrous <- deckCardHolder "Monstrous Transformation"
      esprit <- deckCardHolder "Lady Esprit"
      let
        page1 = setTitle "title" >> p "body1" >> p.green.validate (isJust monstrous) "pureLunacy"
        page2 = p.green.validate (isJust esprit) "atTheCrossroads"
        page3 = do
          p "body2"
          ul $ li.nested "decide" do
            li "focusDefenses"
            li "stretchEquipment"

      for_ monstrous \(iid, card) -> do
        flavor page1
        beginWithCardInHand iid card
      let carried1 = if isJust monstrous then setTitle "title" else page1

      for_ esprit \(iid, card) -> do
        flavor (carried1 >> page2)
        beginWithCardInHand iid card
      let carried2 = if isJust esprit then setTitle "title" else carried1 >> page2

      storyWithChooseOneM (carried2 >> page3) do
        labeled "focusDefenses" $ whenM (selectAny $ ChaosTokenFaceIs Tablet) do
          removeChaosToken Tablet
          addChaosToken Cultist
        labeled "stretchEquipment" $ whenM (selectAny $ ChaosTokenFaceIs Cultist) do
          removeChaosToken Cultist
          addChaosToken Tablet
      pure s
    Setup -> runScenarioSetup ThousandToOne attrs do
      rallies <- getHasRecord TheCultRallies
      destinies <- getDestinies

      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "shubNiggurath"
        li "ravenousBrood"
        li "destinies"
        li.nested "checkDoom" do
          li.validate rallies "cultRallies"
        li "setAside"
        unscoped $ li "shuffleRemainder"

      gather Set.ThousandToOne
      gather Set.ChildrenOfTheGoat
      gather Set.CultOfShubNiggurath
      gather Set.LunaticNight
      gather Set.SavageWoods

      setAgendaDeck [Agendas.underMoonlessSkies]
      setActDeck [Acts.ageOldVisions]

      primalForest <- place Locations.primalForest
      highThicket <- place Locations.highThicket
      place_ Locations.sparseWoodland
      startAt =<< place Locations.silentClearing
      placeAll [Locations.mossyGlen, Locations.fallenCopse]

      enemyAt_ Enemies.shubNiggurath primalForest

      {- One of the eight gathered copies goes into play at High Thicket; "set aside each
      other copy of Ravenous Brood" sends the remaining seven to the pile Shub-Niggurath's
      end-of-round Forced spawns from. -}
      broods <- fromGathered (cardDefIs Enemies.ravenousBrood_209)
      brood <- maybe (genCard Enemies.ravenousBrood_209) pure (listToMaybe broods)
      push $ SetAsideCards (drop 1 broods)
      lead <- getLead
      lift $ chooseOneM lead $ scope "ravenousBrood" do
        -- Both faces are legal here, so the prompt carries the card: the client
        -- renders a question's card flippably, which is the only way to read the
        -- side you are not being shown.
        questionLabeledCard Enemies.ravenousBrood_209
        labeled "spawnHunterSide" $ putBroodIntoPlay brood highThicket
        labeled "spawnAlertSide" $ putBroodIntoPlay (flipCard brood) highThicket

      {- "Check the Campaign Log. For each word recorded under Destinies, find the story
      card with the corresponding word and resolve its story text. Remove each other story
      card from the game." This has to follow the locations: Strike the Heart and Silence
      the Pipes put their backs at Fallen Copse and Mossy Glen, which they read off the
      board. The Destiny stories are double-sided, so they are gathered beside the
      encounter deck rather than into it -- 'removeCards' is what takes one out of the
      game, not 'removeEvery'. -}
      for_ destinyStories \(word, def) -> do
        removeCards =<< amongGathered (cardDefIs def)
        for_ (listToMaybe [iid | (iid, w) <- destinies, w == word]) \iid -> do
          card <- genCard def
          lift $ resolveStory iid card

      -- "Check the Campaign Log. If the cult rallies, place 2 doom on agenda 1a."
      when rallies $ placeDoomOnAgenda 2

      setAside [Agendas.theProphecyFulfilled, Agendas.theProphecyUnfulfilled]
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      moonTokenRider attrs iid token.face
      -- Hard/Expert drops the "if you fail" clause, so the elder thing seals a ☾ even on a
      -- test that succeeded.
      when (token.face == ElderThing && isHardExpert attrs) $ sealMoonTokenOn iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      moonTokenRider attrs iid token.face
      when (token.face == ElderThing) $ sealMoonTokenOn iid
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R1
        Resolution 1 -> do
          resolution "resolution1"
          record TheInvestigatorsWereDevouredByTheThousandYoung
          record ShubNiggurathReignsOverAnEclipsedWorld
          selectEach UneliminatedInvestigator $ push . InvestigatorKilled (toSource attrs)
          gameOver
          endOfScenario
        Resolution 2 -> do
          record TheInvestigatorsEndedTheRitual
          record ShubNiggurathVanishedWithTheEclipse
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs (toBonus "savedTheWorld" 5)
          -- "Each investigator suffers any combination of 3 physical and/or mental trauma."
          -- The scenario runner already answers the "Purchase Trauma" amounts target.
          eachInvestigator \iid ->
            chooseAmounts
              iid
              ("$" <> ikey "trauma")
              (TotalAmountTarget 3)
              [("$physical", (0, 3)), ("$mental", (0, 3))]
              (LabeledTarget "Purchase Trauma" (toTarget attrs))
          -- The investigators win: the campaign carries on to its Epilogue.
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> ThousandToOne <$> liftRunMessage msg attrs
