module Arkham.Scenarios.TracesToNowhere.Helpers where

import Arkham.Ability
import Arkham.Agenda.Types (AgendaAttrs)
import Arkham.Asset.Types qualified as Asset
import Arkham.Calculation
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Deck qualified as Deck
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Enemy.Types (EnemyAttrs)
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty))
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Window (getAsset)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Layout
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Location.Types (LocationAttrs)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message hiding (AssetDefeated)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.SkillType
import Arkham.Source
import Arkham.Story.Types (StoryAttrs)
import Arkham.Target
import Arkham.Trait (Trait (Arkham, Witness))
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Treacheries
import Arkham.Window (Window)
import Data.Aeson.Key qualified as Key
import Data.List.NonEmpty qualified as NE

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "tracesToNowhere" a

{- | Arkham sits in the top three rows: Steer's Office between the two districts,
the church and the society below them, and the funeral home beneath both. Every
printed connection lands on an adjacent cell. The Otherworld chain is a band of
its own at the bottom, entered from the funeral home directly above it.
-}
initialLayout :: [GridTemplateRow]
initialLayout =
  [ "trainStation merchantDistrict steersOffice       frenchHill   arkhamAdvertiser"
  , "tickTockClub southChurch      historicalSociety  meigerEstate ."
  , ".            .                duterteFuneralHome .            ."
  , ".            thePortal        etherealPerch      bonePit      ."
  ]

-- * The Leads deck

--
-- Shares 'LeadsDeck' with The Vanishing of Elina Harper, but is drawn from
-- rather than searched: a card drawn from it resolves as though it had been
-- drawn from the encounter deck, whatever its type.

getLeadsDeck :: HasGame m => m [Card]
getLeadsDeck = getScenarioDeck LeadsDeck

shuffleLeadsDeck :: ReverseQueue m => m ()
shuffleLeadsDeck = shuffleDeck LeadsDeck

shuffleIntoLeadsDeck
  :: (ReverseQueue m, MonoFoldable cards, Element cards ~ card, IsCard card)
  => cards -> m ()
shuffleIntoLeadsDeck cs = shuffleCardsIntoDeck LeadsDeck $ map toCard $ toList cs

-- | Draw the top card of the Leads deck and resolve it as an encounter card.
drawFromLeadsDeck :: ReverseQueue m => InvestigatorId -> m ()
drawFromLeadsDeck iid = do
  getLeadsDeck >>= \case
    [] -> pure ()
    card : _ -> do
      push $ RemoveCardFromScenarioDeck LeadsDeck card
      resolveAsEncounterCard iid card

{- | Resolve a card as though it had been drawn from the top of the encounter
deck. Everything in the Leads deck and everything rescued takes this path.
-}
resolveAsEncounterCard :: ReverseQueue m => InvestigatorId -> Card -> m ()
resolveAsEncounterCard iid card = case card of
  EncounterCard ec -> push $ InvestigatorDrewEncounterCard iid ec
  _ -> pure ()

-- * Abduction

--
-- An abducted card is placed facedown beneath the scenario reference card.

-- | The cards currently abducted.
getAbductedCards :: HasGame m => m [Card]
getAbductedCards = scenarioField ScenarioCardsUnderScenarioReference

-- | Something abducts these cards: they go facedown beneath the scenario reference card.
abduct
  :: (ReverseQueue m, MonoFoldable cards, Element cards ~ card, IsCard card)
  => cards -> m ()
abduct cs = do
  cards <- traverse (setFacedown True . toCard) (toList cs)
  placeUnderneath ScenarioTarget cards

isAbducted :: HasGame m => Card -> m Bool
isAbducted card = elem (toCardId card) . map toCardId <$> getAbductedCards

{- | "{fast} During your turn (except during a skill test), spend 4 resources: You
hire a cab. ... You may spend 1 action to reduce this ability's cost by 2."

Printed verbatim on seven of the Arkham locations, so the two ways to pay are
built once here and labeled -- an unlabeled 'OrCost' would render them as two
bare resource costs with nothing to say which one also spends the action.
-}
hireACab :: LocationAttrs -> Int -> Ability
hireACab a n =
  restricted a n (Here <> DuringTurn You <> not_ DuringAnySkillTest)
    $ FastAbility
    $ OrCost
      [ LabeledCost (scenarioI18n $ "$" <> labelKey "cab.resources") (ResourceCost 4)
      , LabeledCost
          (scenarioI18n $ "$" <> labelKey "cab.actionAndResources")
          (ActionCost 1 <> ResourceCost 2)
      ]

-- | "You hire a cab. Choose an Arkham location with no enemies. Move to that location."
hireACabEffect :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
hireACabEffect iid source = do
  locations <- select $ LocationWithTrait Arkham <> not_ (LocationWithEnemy AnyEnemy)
  chooseTargetM iid locations $ moveTo source iid

{- | "Rescue": draw one card at random from a pile and resolve it by type -- a
story asset to the victory display, a player card to its owner's hand, a
treachery or enemy as though drawn from the encounter deck.

The pile is a parameter because the scenario rescues from two places: the
abducted cards beneath the scenario reference card, and -- once the Otherworld
is in play -- the cards beneath The Portal.
-}
rescueFrom :: ReverseQueue m => InvestigatorId -> Target -> [Card] -> m ()
rescueFrom iid from cards = case cards of
  [] -> pure ()
  _ -> do
    card <- sample (NE.fromList cards)
    push $ RemoveFromUnderneath from [card]
    resolveRescued iid card

-- | Rescue one of the abducted cards at random.
rescue :: ReverseQueue m => InvestigatorId -> m ()
rescue iid = rescueFrom iid ScenarioTarget =<< getAbductedCards

resolveRescued :: ReverseQueue m => InvestigatorId -> Card -> m ()
resolveRescued iid card = do
  card' <- setFacedown False card
  if card'.kind == EncounterAssetType
    then addToVictory_ card'
    else case card' of
      PlayerCard _ -> addToHand iid [card']
      _ -> resolveAsEncounterCard iid card'

{- | "Something abducts <an asset in play>": the asset leaves play and its card
joins the abducted pile beneath the scenario reference card.
-}
abductAsset :: ReverseQueue m => AssetId -> m ()
abductAsset aid = do
  card <- field Asset.AssetCard aid
  push $ RemovedFromPlay (AssetSource aid)
  abduct [card]

{- | "Forced - When a story asset would be defeated: Something abducts that asset,
instead." Printed on agendas 1a, 2a and 3a, so it is built once.

Story assets here are encounter-backed, which is what distinguishes them from a
player's Ally; the defeat itself is cancelled by cancelling its window batch.
-}
abductInsteadOfDefeat :: AgendaAttrs -> Int -> Ability
abductInsteadOfDefeat a n =
  mkAbility a n $ forced $ AssetDefeated #when ByAny (not_ AssetNonStory)

-- | The other half of 'abductInsteadOfDefeat', run from the ability's handler.
abductInsteadOfDefeatEffect :: ReverseQueue m => [Window] -> m ()
abductInsteadOfDefeatEffect ws = do
  cancelWindowBatch ws
  abductAsset (getAsset ws)

{- | "Forced - After doom is placed on this agenda: The investigator with the most
clues must reveal a random chaos token from the chaos bag. If a {skull},
{cultist}, {tablet}, {elderThing}, or {autoFail} is revealed, spawn the set-aside
Abductor enemy engaged with that investigator. If it is already in play, it
attacks that investigator instead."

Printed identically on agendas 2a and 3a.
-}
abductorDoomWatch :: AgendaAttrs -> Int -> Ability
abductorDoomWatch a n = mkAbility a n $ forced $ PlacedDoomCounter #after AnySource (targetIs a)

-- | The symbol tokens that summon the Abductor.
abductorTokens :: [ChaosTokenFace]
abductorTokens = [Skull, Cultist, Tablet, ElderThing, AutoFail]

-- | 'abductorDoomWatch' part one: the investigator with the most clues reveals a token.
abductorDoomWatchRequest :: (ReverseQueue m, Sourceable source) => source -> m ()
abductorDoomWatchRequest source =
  selectOne MostClues >>= traverse_ \iid -> requestChaosTokens_ iid source 1

{- | 'abductorDoomWatch' part two: on a symbol token the Abductor spawns engaged
with that investigator, or attacks them if it is already in play.
-}
abductorDoomWatchResolve
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> [ChaosToken] -> m ()
abductorDoomWatchResolve source iid tokens = do
  when (any ((`elem` abductorTokens) . (.face)) tokens) do
    selectOne (enemyIs Enemies.abductorThatWhichLurksInShadows) >>= \case
      Just abductor -> initiateEnemyAttack abductor source iid
      Nothing -> do
        card <- fetchCard Enemies.abductorThatWhichLurksInShadows
        createEnemy_ card (SpawnEngagedWith iid)
  resetChaosTokens source

{- | The three Culprit enemies (Albrecht, Eugene, Lina) print the same two
clauses and the same story back, differing only in the skill tested and in the
condition that opens the parley.

"Forced - When the enemy phase ends, if Dimensional Shambler is at <X>'s
location: <X> takes 1 damage."
-}
culpritShamblerDamage :: EnemyAttrs -> Int -> Ability
culpritShamblerDamage a n =
  restricted
    a
    n
    (thisExists a $ EnemyAt (LocationWithEnemy (enemyIs Enemies.dimensionalShamblerDraggingThemBelow)))
    $ forced
    $ PhaseEnds #when #enemy

{- | The Culprit story back, which all three print identically apart from the
skill: "Choose an investigator at <X>'s location to test <skill> (4). Before
revealing chaos tokens, investigators at <X>'s location may spend 1-4 clues, as
a group. For each clue spent this way, reduce the difficulty of this test by 1."

Split across three calls because the clue spend happens before the test and the
two branches resolve after it; the story module wires them to its own
'ResolveThisStory', 'PassedThisSkillTest' and 'FailedThisSkillTest'.
-}
culpritParleyBegin :: ReverseQueue m => StoryAttrs -> SkillType -> m ()
culpritParleyBegin attrs skill = do
  culpritOf attrs >>= traverse_ \enemy -> withLocationOf enemy \lid -> do
    here <- select (investigatorAt lid)
    spendable <- getSpendableClueCount here
    leadChooseOrRunOneM $ targets here \iid -> do
      sid <- getRandom
      chooseOneM iid $ scenarioI18n do
        for_ [0 .. min 4 spendable] \n ->
          countVar n $ labeled "culprit.spendClues" do
            when (n > 0) $ push $ SpendClues n here
            beginSkillTest sid iid attrs enemy skill (Fixed $ 4 - n)

-- | "flip this card over and move <X> to The Portal (or to any Arkham location, if he is at The Portal)."
culpritParleySuccess :: ReverseQueue m => StoryAttrs -> InvestigatorId -> m ()
culpritParleySuccess attrs iid =
  culpritOf attrs >>= traverse_ \enemy -> do
    atPortal <- enemy <=~> EnemyAt (locationIs Locations.thePortal)
    destinations <-
      select $ if atPortal then LocationWithTrait Arkham else locationIs Locations.thePortal
    chooseTargetM iid destinations $ enemyMoveTo attrs enemy

{- | "flip this card over and <X> attacks that investigator, disengages from all
investigators, and moves directly to the Dimensional Shambler's location."
-}
culpritParleyFailure :: ReverseQueue m => StoryAttrs -> InvestigatorId -> m ()
culpritParleyFailure attrs iid =
  culpritOf attrs >>= traverse_ \enemy -> do
    initiateEnemyAttack enemy attrs iid
    selectEach (investigatorEngagedWith enemy) (`disengageEnemy` enemy)
    shambler <- selectJust $ enemyIs Enemies.dimensionalShamblerDraggingThemBelow
    withLocationOf shambler $ enemyMoveTo attrs enemy

-- | The enemy whose back this story is.
culpritOf :: HasGame m => StoryAttrs -> m (Maybe EnemyId)
culpritOf attrs = selectOne $ EnemyWithCardId attrs.cardId

{- | The chaos token faces currently sealed on the scenario reference card. Act 2b
branches on which pair of symbols the codex entries sealed there.
-}
getSealedOnScenarioReference :: HasGame m => m [ChaosTokenFace]
getSealedOnScenarioReference = map (.face) <$> scenarioField ScenarioSealedChaosTokens

{- | Act 1b sends the Abductor to a Witness: "the same location as the Witness
story asset farthest from each investigator (or, if Abductor is already in play,
move it to that location)".

Returns 'False' when no Witness is in play, which is the act's cue to reveal from
the Leads deck instead.
-}
sendAbductorToAWitness :: (ReverseQueue m, Sourceable source) => source -> m Bool
sendAbductorToAWitness source = do
  locations <- select $ FarthestLocationFromAll (LocationWithAsset $ AssetWithTrait Witness)
  case locations of
    [] -> pure False
    _ -> do
      lead <- getLead
      chooseOrRunOneM lead $ targets locations (placeAbductorAt source)
      pure True

{- | The other half: "reveal cards from the top of the leads deck until a Witness
story asset is revealed. Resolve that asset's revelation effect ... Shuffle each
other card revealed this way into the leads deck."
-}
revealLeadsUntilWitness :: ReverseQueue m => InvestigatorId -> m ()
revealLeadsUntilWitness iid = do
  deck <- getLeadsDeck
  let (skipped, found) = break (`cardMatch` CardWithTrait Witness) deck
  for_ skipped $ push . RemoveCardFromScenarioDeck LeadsDeck
  for_ (listToMaybe found) \card -> do
    push $ RemoveCardFromScenarioDeck LeadsDeck card
    resolveAsEncounterCard iid card
  shuffleIntoLeadsDeck skipped

-- | Put the Abductor at a location, spawning it from set aside or moving it there.
placeAbductorAt :: (ReverseQueue m, Sourceable source) => source -> LocationId -> m ()
placeAbductorAt source lid =
  selectOne (enemyIs Enemies.abductorThatWhichLurksInShadows) >>= \case
    Just abductor -> enemyMoveTo source abductor lid
    Nothing -> do
      card <- fetchCard Enemies.abductorThatWhichLurksInShadows
      createEnemyAt_ card lid

{- | A Chapter 2 codex entry: title and body inside the codex frame. The scenario's
ten entries are all reached from a Suspect or Witness parley.
-}
codexEntry :: (HasI18n, ReverseQueue m) => Scope -> m ()
codexEntry x = scope x $ flavor $ setTitle "title" >> compose.codex (h "title" >> p "body")

{- | Codex D and G print two displays of one entry. The first trigger reads
display one and then proceeds to display two; every later trigger skips straight
to display two. Both displays keep the routing lines the entry prints above the
split, so each reads as the whole codex page, and display two carries the entry's
own choices as buttons on the panel rather than as a question after it.
-}
codexBody :: HasI18n => Scope -> Scope -> FlavorTextBuilder ()
codexBody x part =
  scope x $ setTitle "title" >> compose.codex (h "title" >> scope part (p "body"))

codexDisplay :: (HasI18n, ReverseQueue m) => Scope -> Scope -> m ()
codexDisplay x part = flavor $ codexBody x part

codexReadKey :: Text -> Key.Key
codexReadKey name = Key.fromText $ "tracesToNowhere.codexRead." <> name

{- | Whether a codex has been triggered before this game. The flag outlives the
parley that set it and the asset may be added to the victory display, so it lives
in scenario meta rather than on the card.
-}
hasReadCodex :: HasGame m => Text -> m Bool
hasReadCodex name = getScenarioMetaKeyDefault (codexReadKey name) False

markCodexRead :: ReverseQueue m => Text -> m ()
markCodexRead name = setGlobal ScenarioTarget (codexReadKey name) True

{- | "If this is the first time this codex has been triggered during this game,
proceed to <X> 1. Otherwise, skip to <X> 2." Display one always falls through to
display two, which carries the entry's mechanical half as buttons on the panel
itself. The choices go to the parleying investigator's player, not the lead's,
because they spend that investigator's resources.
-}
twoPartCodex
  :: (HasI18n, ReverseQueue m) => InvestigatorId -> Scope -> Text -> ChooseT m a -> m ()
twoPartCodex iid x name choices = do
  unlessM (hasReadCodex name) do
    markCodexRead name
    codexDisplay x "first"
  investigatorStoryWithChooseOneM' iid (codexBody x "second") choices

{- | The Witness parley, printed the same on all four: "Parley. Test <skill> (4).
You may spend 1 clue to reduce the difficulty of this test by 3."
-}
witnessParley
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId -> InvestigatorId -> source -> SkillType -> m ()
witnessParley sid iid source skill = do
  clues <- field InvestigatorClues iid
  when (clues > 0) do
    chooseOneM iid $ scenarioI18n do
      labeled "witness.keepClue" nothing
      labeled "witness.spendClue" do
        spendClues iid 1
        skillTestModifier sid source sid (Difficulty (-3))
  beginSkillTest sid iid source iid skill (Fixed 4)

{- | A codex entry's reward: "Search the chaos bag for a <symbol> token and seal
it on the scenario reference card. Add <witness> to the victory display."
-}
sealSymbolAndClaim :: (ReverseQueue m, Targetable a) => a -> ChaosTokenFace -> m ()
sealSymbolAndClaim witness face = do
  selectOne (ChaosTokenFaceIs face) >>= traverse_ (sealChaosToken_ ScenarioTarget)
  addToVictory_ witness

{- | Both Otherworld doors print the same three lines: "If the Ethereal Perch
location is set aside, put it into play. Move each investigator at this location
to Ethereal Perch. Shuffle any remaining set-aside copies of Lost to the
Darkness into the encounter deck."
-}
descendToEtherealPerch
  :: (ReverseQueue m, Sourceable source) => source -> CardDef -> m ()
descendToEtherealPerch source from = do
  placeLocationIfNotInPlay_ Locations.etherealPerch
  selectForMaybeM (locationIs Locations.etherealPerch) \perch ->
    selectEach (investigatorAt (locationIs from)) \iid -> moveTo source iid perch
  shuffleCardsIntoDeck Deck.EncounterDeck
    =<< getSetAsideCardsMatching (cardIs Treacheries.lostToTheDarkness)

{- | Tick-Tock Club's blackjack: "Treat each {skull}, {cultist}, {tablet}, or
{elderThing} as a 5; the {autoFail} as a 10; and the {elderSign} as either a 1
or an 11. If the combined value of the revealed tokens (ignoring +/-) is..."

Numeric tokens count their printed value with the sign dropped, which is what
"ignoring +/-" means.
-}
blackjackValue :: ChaosTokenFace -> [Int]
blackjackValue = \case
  ElderSign -> [1, 11]
  AutoFail -> [10]
  Skull -> [5]
  Cultist -> [5]
  Tablet -> [5]
  ElderThing -> [5]
  other -> [abs other.faceValue]

-- | The payout table, or 'Nothing' on a bust.
blackjackPayout :: Int -> Maybe Int
blackjackPayout total
  | total <= 18 = Just 3
  | total == 19 = Just 4
  | total == 20 = Just 5
  | total == 21 = Just 8
  | otherwise = Nothing
