{-# OPTIONS_GHC -Wno-orphans #-}

{- | Effect dispatch for the "Ultimatums and Boons" variant system.

Selected entries live on 'Arkham.Game.Settings.Settings'; every hook in this
module (and at the few external call sites: 'Arkham.Game.preloadModifiers',
@instance HasAbilities Game@, the @InitDeck@ handlers) reads through
'getActiveUltimatumsAndBoons', so the runtime enable/disable toggle
('SetUltimatumsAndBoonsEnabled') gates everything uniformly.

Entries follow the tarot-card pattern ("Arkham.Scenario"): they are not cards
or entities, just values with a 'Source', whose modifiers fan in during
modifier preload, whose abilities fan in via @HasAbilities Game@, and whose
ability uses are dispatched from the scenario's @RunMessage@ through
'runUltimatumsAndBoonsMessage'.
-}
module Arkham.UltimatumsAndBoons (
  module Arkham.UltimatumsAndBoons,
  module Arkham.UltimatumsAndBoons.Types,
) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Agenda.CardDefs.TheDreamEaters.WhereTheGodsDwell qualified as Agendas
import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheDrownedCity.Helpers (expeditionItems)
import Arkham.Campaigns.TheDrownedCity.Key (TheDrownedCityKey (SpoiledExpeditionItems))
import Arkham.Campaigns.ThePathToCarcosa.Key (ThePathToCarcosaKey (YouHeadedDanielsWarning))
import Arkham.Card
import Arkham.ChaosToken.Types (ChaosTokenFace (..))
import Arkham.Classes.HasGame
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasQueue
import Arkham.Classes.Query (select, selectAny, selectCount, selectOne, (<=~>))
import Arkham.Criteria qualified as Criteria
import Arkham.Deck qualified as Deck
import Arkham.Decklist.RandomBasicWeakness (
  RandomBasicWeaknessContext (..),
  sampleRandomBasicWeakness,
 )
import Arkham.DefeatedBy
import Arkham.EncounterSet (EncounterSet (Tekelili))
import Arkham.Enemy.CardDefs.TheDunwichLegacy.UndimensionedAndUnseen qualified as Enemies
import Arkham.Enemy.Helpers (cancelEnemyDefeat)
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Game.Base
import Arkham.Game.Settings
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.ChaosToken (cancelChaosToken)
import Arkham.Helpers.Log (getHasRecord, recordSetInsert, scenarioCount)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario (getEncounterDeck, getIsStandalone)
import Arkham.Helpers.Window (checkWindows)
import Arkham.Helpers.Window.Enemy (defeatedEnemy)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (
  Field (..),
  Investigator,
  investigatorHealthDamage,
  investigatorSanityDamage,
 )
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Message.Lifted (
  advanceToAgendaA,
  createEnemyAt_,
  discardTopOfEncounterDeck,
  exhaustWith,
  focusCards,
 )
import Arkham.Message.Lifted.Card (drawEncounterCard, playCardPayingCostWithWindows)
import Arkham.Message.Lifted.Damage (healAllDamage)
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey (ScenarioCountKey (CthulhuRage))
import Arkham.Source
import Arkham.Target
import Arkham.Trait (Trait (Ally, Artifact, Elite, Enraged, Humanoid))
import Arkham.Treachery.CardDefs.TheForgottenAge.Poison qualified as Treacheries
import Arkham.UltimatumsAndBoons.Types
import Arkham.Window (mkAfter, revealedChaosTokens)
import Arkham.Window qualified as Window
import Arkham.Xp
import Data.Aeson.Key qualified as Key
import Data.Text qualified as T

{- | Selected entries, or the empty set while the runtime toggle is disabled.
The single gate every hook must read through.
-}
getActiveUltimatumsAndBoons :: HasGame m => m (Set UltimatumOrBoon)
getActiveUltimatumsAndBoons = activeUltimatumsAndBoons . gameSettings <$> getGame

hasUltimatumOrBoon :: HasGame m => UltimatumOrBoon -> m Bool
hasUltimatumOrBoon b = member b <$> getActiveUltimatumsAndBoons

hasBoon :: HasGame m => Boon -> m Bool
hasBoon = hasUltimatumOrBoon . Boon

hasUltimatum :: HasGame m => Ultimatum -> m Bool
hasUltimatum = hasUltimatumOrBoon . Ultimatum

fromUltimatumOrBoon :: UltimatumOrBoon -> SourceableWithCardCode
fromUltimatumOrBoon b = SourceableWithCardCode (CardCode $ variantName b) (UltimatumOrBoonSource b)

isUltimatumOrBoonSource :: Source -> Bool
isUltimatumOrBoonSource = \case
  UltimatumOrBoonSource _ -> True
  _ -> False

{- | The one card Boon of the Child may play: the topmost event in that
investigator's discard pile, if it is playable. One definition shared by the
ability's criteria and its handler so the two can never disagree.
-}
boonOfTheChildCard :: Matcher.InvestigatorMatcher -> Matcher.ExtendedCardMatcher
boonOfTheChildCard who =
  Matcher.PlayableCard (UnpaidCost NeedsAction)
    $ Matcher.TopmostOfDiscardOf who (Matcher.CardWithType EventType)

{- | Marks an investigator whose first autofail of the game has already
resolved. Boon of Athena is "the first time each game": declining the offer
forfeits it, so the ability requires this marker absent, and the marker is
set when an autofail reveal actually resolves (a cancelled reveal never
reaches the RevealChaosToken message, so using the boon doesn't set it — the
per-game ability limit covers that side).
-}
boonOfAthenaExpiredMarker :: ModifierType
boonOfAthenaExpiredMarker = MetaModifier "revealedFirstAutofail"

instance HasModifiersFor UltimatumOrBoon where
  getModifiersFor = \case
    Ultimatum u -> getModifiersFor u
    Boon b -> getModifiersFor b

instance HasModifiersFor Ultimatum where
  getModifiersFor u = do
    let source = UltimatumOrBoonSource (Ultimatum u)
    case u of
      UltimatumOfHardship ->
        modifySelectWith source Matcher.Anyone setActiveDuringSetup [StartingResources (-2)]
      UltimatumOfForbiddenKnowledge ->
        modifySelectWith source Matcher.Anyone setActiveDuringSetup [StartingHand (-1)]
      UltimatumOfInduction -> modifySelectMaybe source Matcher.Anyone \_ -> do
        liftGuardM $ not <$> getIsStandalone
        pure [CannotGainXP]
      {- Ultimatum of Invisibility: the Brood already refuses everything but
      Esoteric Formula's own attacks and damage; this widens that to every kind
      of player-card effect, and makes it Elite. -}
      UltimatumOfInvisibility ->
        -- ScenarioMatcher has no OneOf instance, and ScenarioWithId does not
        -- fall back to the scenario's reference, so the Return-to id is listed.
        whenM (anyM (selectAny . Matcher.ScenarioWithId) ["02236", "51041"]) do
          modifySelect source (Matcher.EnemyWithTitle "Brood of Yog-Sothoth")
            $ AddTrait Elite
            : [ CannotBeAttackedByPlayerSourcesExcept exceptFormula
              , CannotBeEvadedByPlayerSourcesExcept exceptFormula
              , CannotBeDamagedByPlayerSourcesExcept exceptFormula
              , CannotBeEngagedByPlayerSourcesExcept exceptFormula
              , CannotReceiveModifiersFromPlayerSources
              , CannotBeExhaustedBy Matcher.SourceIsPlayerCard
              , CannotBeDefeatedBy Matcher.SourceIsPlayerCard
              , CannotBeRemovedBy Matcher.SourceIsPlayerCard
              , CannotBeMovedBy Matcher.SourceIsPlayerCard
              , CannotBeDisengagedBy Matcher.SourceIsPlayerCard
              ]
      {- Ultimatum of The Man. Corpse Dweller finds its host with
      @EnemyWithTrait Humanoid@, which reads the modified traits field, so
      dropping the trait is enough -- no edit to its module. The trait is gone
      for every other lookup too, but nothing else in the campaign asks. -}
      UltimatumOfTheMan -> whenM (selectAny $ Matcher.ScenarioWithId "03240") do
        modifySelect source theMan [RemoveTrait Humanoid]
        whenM (selectAny $ Matcher.ActWithStep 2) do
          modifySelect source theMan [CannotMove, CannotBeMoved]
      -- Ultimatum of the Drowned: "each agenda gets -1 doom threshold." The two
      -- Awakened-and-Enraged cards carry the rest, keyed on the ultimatum itself.
      UltimatumOfTheDrowned -> whenM (selectAny $ Matcher.ScenarioWithId "07311") do
        modifySelect source Matcher.AnyAgenda [DoomThresholdModifier (-1)]
      -- Ultimatum of Death: "Agenda 2a gains +6 doom threshold."
      UltimatumOfDeath -> whenM (anyM (selectAny . Matcher.ScenarioWithId) ["03240", "52048"]) do
        modifySelect source (Matcher.AgendaWithId "03242") [DoomThresholdModifier 6]
      {- Ultimatum of Spoilage: no abilities on Artifacts and no damage assigned
      to them. The damage half is a per-investigator modifier on the asset, so
      it is built one investigator at a time. -}
      UltimatumOfSpoilage -> whenM ((== Just "11") <$> selectOne Matcher.TheCampaign) do
        modifySelect
          source
          Matcher.Anyone
          [CannotTriggerAbilityMatching $ Matcher.AbilityOnAsset (Matcher.AssetWithTrait Artifact)]
        iids <- select Matcher.Anyone
        for_ iids \iid ->
          modifySelect source (Matcher.AssetWithTrait Artifact) [CannotAssignDamage iid]
      {- Ultimatum of the Sleeper: each Enraged Cthulhu's health, printed X and
      resolved to Cthulhu's Rage by the facet itself, becomes that per player.
      Health modifiers add, so this contributes the extra players' worth. -}
      UltimatumOfTheSleeper -> whenM (selectAny $ Matcher.ScenarioWithId "11688a") do
        rage <- scenarioCount CthulhuRage
        playerCount <- getPlayerCount
        when (rage > 0 && playerCount > 1) do
          modifySelect
            source
            (Matcher.EnemyWithTrait Enraged)
            [HealthModifier (rage * (playerCount - 1))]
      UltimatumOfTheScream -> do
        screamed <- settingsScreamedAllies . gameSettings <$> getGame
        unless (null screamed) do
          modifySelect
            source
            Matcher.Anyone
            [CannotPlay $ Matcher.mapOneOf Matcher.CardWithCardCode (toList screamed)]
      _ -> pure ()
   where
    exceptFormula =
      Matcher.oneOf
        [Matcher.SourceIsAbility Matcher.BasicAbility, Matcher.SourceIsAsset (Matcher.AssetIs "02254")]
    theMan = Matcher.EnemyWithTitle "The Man in the Pallid Mask"

instance HasModifiersFor Boon where
  getModifiersFor b = do
    let source = UltimatumOrBoonSource (Boon b)
    case b of
      BoonOfHades ->
        modifySelectWith source Matcher.Anyone setActiveDuringSetup [StartingResources 2]
      BoonOfThoth ->
        modifySelectWith source Matcher.Anyone setActiveDuringSetup [StartingHand 1]
      BoonOfHermes ->
        modifySelect
          source
          Matcher.Anyone
          [ GiveAdditionalAction
              $ AdditionalAction "Boon of Hermes" source (ActionRestrictedAdditionalAction #move)
          ]
      BoonOfTheExplorer ->
        modifySelect
          source
          Matcher.Anyone
          [ GiveAdditionalAction
              $ AdditionalAction "Boon of the Explorer" source (ActionRestrictedAdditionalAction #explore)
          ]
      BoonOfPersephone -> modifySelectMaybe source Matcher.DefeatedInvestigator \_ -> do
        liftGuardM $ not <$> getIsStandalone
        pure [XPModifier "Boon of Persephone" 3]
      -- Refractions: both only bend their own scenario's agendas.
      BoonOfAtonement -> whenM (selectAny $ Matcher.ScenarioWithId "09660") do
        modifySelect source Matcher.AnyAgenda [DoomThresholdModifier 1]
      BoonOfBliss -> whenM (selectAny $ Matcher.ScenarioWithId "10651") do
        modifySelect source Matcher.AnyAgenda [DoomThresholdModifier 2]
      -- Boon of The Dreamer: "this agenda gets +2 doom threshold" -- agenda 3a
      -- only, which is the one the boon advances to.
      BoonOfTheDreamer -> whenM (selectAny $ Matcher.ScenarioWithId "06286") do
        modifySelect source (Matcher.AgendaWithId "06289") [DoomThresholdModifier 2]
      _ -> pure ()

ultimatumOrBoonAbilities :: UltimatumOrBoon -> [Ability]
ultimatumOrBoonAbilities = \case
  Ultimatum u -> ultimatumAbilities u
  Boon b -> boonAbilities b

{- | 'HasAbilities' is pure, so an ability cannot ask which campaign is being
played: the criteria have to make it unreachable elsewhere instead. Both of
these are self-limiting -- Poisoned and exploration only exist in The Forgotten
Age.
-}
ultimatumAbilities :: Ultimatum -> [Ability]
ultimatumAbilities u = case u of
  -- "Each copy of Poisoned gains 'Forced - When the game ends, if you have not
  -- been eliminated: Suffer 1 physical trauma.'"
  UltimatumOfVenom ->
    [ restricted
        (fromUltimatumOrBoon (Ultimatum u))
        1
        (exists $ Matcher.HasMatchingTreachery (Matcher.treacheryIs Treacheries.poisoned))
        $ forced (Matcher.GameEnds #when)
    ]
  -- "After each successful exploration, the performing investigator reveals the
  -- top card of the encounter deck..."
  UltimatumOfAmbuscade ->
    [ restricted (fromUltimatumOrBoon (Ultimatum u)) 1 Criteria.NoRestriction
        $ forced
        $ Matcher.Explored #after Matcher.You Matcher.Anywhere (Matcher.SuccessfulExplore Matcher.Anywhere)
    ]
  {- Ultimatum of Death: "Specter of Death gains 'Forced - When Specter of Death
  is defeated: Instead of adding it to the victory display, heal all damage from
  it and exhaust it. It does not ready during the next upkeep phase.'" -}
  UltimatumOfDeath ->
    [ restricted
        (fromUltimatumOrBoon (Ultimatum u))
        1
        (Criteria.ScenarioExists $ Matcher.ScenarioWithId "03240")
        $ forced
        $ Matcher.EnemyWouldBeDefeated #when (Matcher.EnemyWithTitle "Specter of Death")
    ]
  _ -> []

boonAbilities :: Boon -> [Ability]
boonAbilities b = case b of
  {- Boon of The Dreamer: "after advancing to Act 5, advance to agenda 3a and
  remove all doom from it." Act 5 is already in play when act 4's advance window
  closes, so this fires off the act that brings it out. -}
  BoonOfTheDreamer ->
    [ restricted
        (fromUltimatumOrBoon (Boon b))
        1
        (Criteria.ScenarioExists $ Matcher.ScenarioWithId "06286")
        $ forced
        $ Matcher.ActAdvances #after (Matcher.ActWithId "06293")
    ]
  BoonOfAthena ->
    [ withTooltip
        "Boon of Athena: cancel the autofail token, return it to the chaos bag, and draw another in its place"
        $ playerLimit PerGame
        $ restricted
          (fromUltimatumOrBoon (Boon b))
          1
          (exists $ Matcher.You <> not_ (Matcher.InvestigatorWithModifier boonOfAthenaExpiredMarker))
        $ freeReaction (Matcher.RevealChaosToken #cancel Matcher.You #autofail)
    ]
  BoonOfDestiny ->
    [ withTooltip "Boon of Destiny: search your deck for 1 copy of a card and add it to your hand"
        $ playerLimit PerGame
        $ mkAbility (fromUltimatumOrBoon (Boon b)) 1
        $ freeReaction (Matcher.DrawingStartingHand #when Matcher.You)
    ]
  {- An explicit ability rather than a CanPlayTopmostOfDiscard permission: the boon
  has to know which play was its own to bottom-deck the event and to spend its use,
  and nothing about a card sitting on top of the discard says that. Double, Double
  replays an event its own first resolution just discarded, so inferring the boon
  from "the played card is the topmost event of the discard" fired on that replay
  (#5768); De Vermis Mysteriis (2), Wendy's Amulet and Marion Tavares are the same
  shape. Recipe is Eldritch Tongue's: the player initiates, the handler attaches the
  riders to that one play. "An investigator may play" is group-wide, hence groupLimit.
  -}
  BoonOfTheChild ->
    [ withTooltip
        "Boon of the Child: play the topmost event in your discard pile as if it were in your hand"
        $ groupLimit PerRound
        $ fastAbility (fromUltimatumOrBoon (Boon b)) 1 Free
        $ exists (boonOfTheChildCard Matcher.You)
    ]
  BoonOfOsiris ->
    [ withTooltip "Boon of Osiris: after suffering trauma, heal all damage and horror"
        $ playerLimit PerGame
        $ mkAbility (fromUltimatumOrBoon (Boon b)) 1
        $ forced (Matcher.InvestigatorWouldBeDefeated #when Matcher.ByAny Matcher.You)
    ]
  _ -> []

{- | Ultimatum of The Scream: remove screamed allies (and all copies) from
every stored campaign deck. Called from the campaign's NextCampaignStep
handler — the scenario-side dispatcher can't host this because campaign
transitions may run in campaign-only mode (no scenario to dispatch), and
EndOfScenario's own handler clears the queue. Idempotent, so re-firing on
every step transition is harmless.
-}
screamedAllyCleanupMessages :: HasGame m => [InvestigatorId] -> m [Message]
screamedAllyCleanupMessages iids = do
  active <- hasUltimatum UltimatumOfTheScream
  screamed <- settingsScreamedAllies . gameSettings <$> getGame
  pure
    [ RemoveCampaignCardFromDeck iid def
    | active
    , code <- toList screamed
    , def <- maybeToList (lookupCardDef code)
    , iid <- iids
    ]

{- | Message dispatch for the interactive entries; called from the scenario's
@RunMessage@ catch-all (mirroring how tarot ability uses are dispatched).
-}
runUltimatumsAndBoonsMessage
  :: (HasGame m, HasQueue Message m, CardGen m)
  => Message
  -> m ()
runUltimatumsAndBoonsMessage msg = case msg of
  UseAbility _ ab _ | isUltimatumOrBoonSource ab.source -> push $ Do msg
  -- Ultimatum of the Broken Veil: weaknesses milled off the top of a deck
  -- shuffle back in.
  DiscardedTopOfDeck iid cards _ _ -> do
    whenM (hasUltimatum UltimatumOfTheBrokenVeil) do
      let weaknesses = filter (`cardMatch` Matcher.WeaknessCard) cards
      unless (null weaknesses) do
        push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) (map PlayerCard weaknesses)
  InvestigatorIsDefeated source iid -> do
    standalone <- getIsStandalone
    unless standalone do
      -- Ultimatum of Finality: defeat by damage kills, defeat by horror
      -- drives insane.
      whenM (hasUltimatum UltimatumOfFinality) do
        attrs <- getAttrs @Investigator iid
        modifiedHealth <- field InvestigatorHealth iid
        modifiedSanity <- field InvestigatorSanity iid
        when (investigatorHealthDamage attrs >= modifiedHealth) $ push $ InvestigatorKilled source iid
        when (investigatorSanityDamage attrs >= modifiedSanity) $ push $ DrivenInsane iid
      -- Ultimatum of the Spiral: a defeated investigator's deck gains a
      -- random basic weakness.
      whenM (hasUltimatum UltimatumOfTheSpiral) do
        ctx <-
          RandomBasicWeaknessContext
            <$> field InvestigatorClass iid
            <*> getPlayerCount
            <*> field InvestigatorTaboo iid
            <*> field InvestigatorCardPool iid
            <*> getIsStandalone
        weakness <- genCard =<< sampleRandomBasicWeakness ctx
        push $ AddCampaignCardToDeck iid DoNotShuffleIn weakness
  -- Ultimatum of The Scream: a defeated unique non-story, non-weakness ally
  -- is removed from the game and banned for the rest of the campaign.
  When (AssetDefeated _ aid) -> do
    -- Ultimatum of Spoilage (see the Discarded arm below for the other half).
    whenM (hasUltimatum UltimatumOfSpoilage) do
      spoilExpeditionItem =<< fieldMap AssetCard toCardDef aid
    standalone <- getIsStandalone
    unless standalone do
      whenM (hasUltimatum UltimatumOfTheScream) do
        screams <-
          aid
            <=~> ( Matcher.UniqueAsset
                     <> Matcher.AssetWithTrait Ally
                     <> Matcher.AssetNonStory
                     <> Matcher.NonWeaknessAsset
                     <> Matcher.AssetControlledBy Matcher.Anyone
                 )
        when screams do
          code <- fieldMap AssetCard toCardCode aid
          replaceMessageMatching
            (\case Do (AssetDefeated _ aid') -> aid == aid'; _ -> False)
            \case
              Do (AssetDefeated _ aid') ->
                [RecordScreamedAlly code, RemoveFromGame (AssetTarget aid')]
              _ -> error "invalid match"
  UseCardAbility iid (UltimatumOrBoonSource (Boon BoonOfAthena)) 1 (revealedChaosTokens -> [token]) _ -> do
    -- SacrificialDoll recipe: ChaosTokenCanceled is what marks the token
    -- cancelled in the bag and strips it from pendingRequests, so the skill
    -- test never receives it; without it the autofail still resolves.
    let source = UltimatumOrBoonSource (Boon BoonOfAthena)
    cancelChaosToken token
    windowMsg <- checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect source Nothing)]
    pushAll
      [ CancelEachNext Nothing source [CheckWindowMessage, DrawChaosTokenMessage, RevealChaosTokenMessage]
      , ChaosTokenCanceled iid source token
      , windowMsg
      , ReturnChaosTokens [token]
      , UnfocusChaosTokens
      , DrawAnotherChaosToken iid
      ]
  UseCardAbility iid source@(UltimatumOrBoonSource (Boon BoonOfDestiny)) 1 _ _ ->
    push $ Msg.search iid source iid [fromDeck] (Matcher.basic Matcher.AnyCard) (AddFoundToHand iid 1)
  UseCardAbility iid source@(UltimatumOrBoonSource (Boon BoonOfOsiris)) 1 ws _ -> do
    let
      defeatedBy =
        headMay
          [ db
          | (Window.windowType -> Window.InvestigatorWouldBeDefeated db iid') <- ws
          , iid' == iid
          ]
      physical = maybe False wasDefeatedByDamage defeatedBy
      mental = maybe False wasDefeatedByHorror defeatedBy
    player <- getPlayer iid
    immunity <- nextTurnModifiers iid source (InvestigatorTarget iid) [CannotBeDamaged]
    let
      traumaMessages
        | physical && mental =
            [ chooseOne
                player
                [ Label (withI18n $ countVar 1 $ ikey' "label.sufferPhysicalTrauma") [SufferTrauma iid 1 0]
                , Label (withI18n $ countVar 1 $ ikey' "label.sufferMentalTrauma") [SufferTrauma iid 0 1]
                ]
            ]
        | physical = [SufferTrauma iid 1 0]
        | mental = [SufferTrauma iid 0 1]
        | otherwise = []
    -- Cheat Death pattern: the queued InvestigatorWhenDefeated sits after the
    -- pending AssignDamage, so the replacement runs once damage has landed —
    -- suffer trauma, then heal everything, then re-check (no longer defeated).
    replaceMessageMatching
      (\case InvestigatorWhenDefeated _ iid' -> iid == iid'; _ -> False)
      \case
        InvestigatorWhenDefeated source' _ ->
          traumaMessages
            <> [ HealAllDamageAndHorror (InvestigatorTarget iid) source
               , immunity
               , Msg.checkDefeated source' iid
               ]
        _ -> error "invalid match"
  RevealChaosToken _ iid token | token.face == AutoFail -> do
    whenM (hasBoon BoonOfAthena) do
      mods <- getModifiers iid
      unless (boonOfAthenaExpiredMarker `elem` mods) do
        push
          =<< gameModifier
            (UltimatumOrBoonSource (Boon BoonOfAthena))
            (InvestigatorTarget iid)
            boonOfAthenaExpiredMarker
  UseCardAbility iid source@(UltimatumOrBoonSource (Boon BoonOfTheChild)) 1 ws _ -> runQueueT do
    cards <- select $ boonOfTheChildCard (Matcher.InvestigatorWithId iid)
    for_ (listToMaybe cards) \card -> do
      -- Scoped to this play, not to "any event played from a discard": that is what
      -- kept other effects from inheriting the bottom-decking. UnlessFastActionCost
      -- keeps the play honest -- a non-fast event still costs an action.
      push
        =<< cardResolutionModifiers
          card
          source
          card
          [PlaceOnBottomOfDeckInsteadOfDiscard, AdditionalCost (UnlessFastActionCost 1)]
      playCardPayingCostWithWindows iid card ws
  {- Ultimatum of Venom. An eliminated investigator is already excluded: a plain
  investigator matcher never matches one. -}
  UseCardAbility _ (UltimatumOrBoonSource (Ultimatum UltimatumOfVenom)) 1 _ _ -> do
    poisoned <- select $ Matcher.HasMatchingTreachery (Matcher.treacheryIs Treacheries.poisoned)
    for_ poisoned \iid -> do
      copies <-
        selectCount
          $ Matcher.treacheryIs Treacheries.poisoned
          <> Matcher.treacheryInThreatAreaOf iid
      pushAll $ replicate copies (SufferTrauma iid 1 0)
  {- The Carcosa pair, both keyed on the one-click HASTUR recorder in the
  scenario UI: it is the only way the engine ever hears a name said at the
  table, and it assigns its 1 horror from 'CampaignSource'. Both are gated on
  Daniel's warning, which is also what keeps them out of other campaigns. -}
  InvestigatorAssignDamage iid CampaignSource _ 0 n | n > 0 -> do
    whenM (getHasRecord YouHeadedDanielsWarning) do
      -- "...in addition to taking 1 horror, suffer 1 mental trauma."
      whenM (hasUltimatum UltimatumOfTheUnspeakableName) $ push (SufferTrauma iid 0 1)
      -- Brass Crown tallies the same presses, per investigator, until its toll.
      whenM (hasUltimatum UltimatumOfTheBrassCrown) $ bumpSpokenHastur iid n
  {- "...spoke, WROTE, or TYPED the name" -- so the log's chat box is a second
  way the engine hears it, and the honour rule should not depend on also
  remembering to press the button. Resolves to exactly the recorder's message,
  so everything above applies unchanged.

  Gated the same way the recorder's button is: Daniel's warning for Carcosa, the
  ultimatum itself for Dark Matter, whose Unspeakable Oath also covers TASSILDA.
  -}
  ChatMessage iid _ text -> do
    carcosa <- getHasRecord YouHeadedDanielsWarning
    oath <- hasUltimatum (HomebrewUltimatum ":dark-matter:UltimatumOfTheUnspeakableOath")
    let said name = name `T.isInfixOf` T.toLower text
    when ((carcosa && said "hastur") || (oath && (said "hastur" || said "tassilda")))
      $ push
      $ InvestigatorAssignDamage iid CampaignSource DamageAny 0 1
  {- Ultimatum of the Brass Crown: "at the beginning of each scenario, take 1
  horror for each time you spoke, wrote, or typed the name of HASTUR since the
  end of the previous scenario." Sourced from the ultimatum rather than the
  campaign so collecting the toll is not itself counted as speaking. -}
  EndSetup -> do
    whenM (hasUltimatum UltimatumOfTheBrassCrown) do
      whenM (getHasRecord YouHeadedDanielsWarning) do
        iids <- select Matcher.Anyone
        for_ iids \iid -> do
          spoken <- spokenHasturCount iid
          when (spoken > 0) do
            push $ Msg.assignHorror iid (fromUltimatumOrBoon (Ultimatum UltimatumOfTheBrassCrown)) spoken
            setSpokenHastur iid 0
    {- Ultimatum of Multiplication: "instead of the standard setup instructions,
    begin the game with all five Brood of Yog-Sothoth cards in play: one in each
    of the five locations besides Dunwich Village." The scenario has already put
    one or two out by now, so this tops the board up rather than replacing the
    setup wholesale. -}
    whenM (hasUltimatum UltimatumOfMultiplication) do
      whenM (anyM (selectAny . Matcher.ScenarioWithId) ["02236", "51041"]) $ runQueueT do
        for_ broodLocationTitles \title -> do
          mlid <- selectOne (Matcher.LocationWithTitle title)
          for_ mlid \lid -> do
            occupied <-
              selectAny $ Matcher.EnemyWithTitle "Brood of Yog-Sothoth" <> Matcher.enemyAt lid
            unless occupied $ createEnemyAt_ Enemies.broodOfYogSothoth lid
    {- Ultimatum of Death: "after setup, immediately advance Agenda 1a to Specter
    of Death and spawn it at your starting location, exhausted." The agenda's own
    side-B handler draws the Specter, which already spawns at position (0,0) by
    its printed text, so only the advance and the exhaust are needed. -}
    whenM (hasUltimatum UltimatumOfDeath) do
      whenM (anyM (selectAny . Matcher.ScenarioWithId) ["03240", "52048"]) do
        push $ AdvanceAgendaBy "03241" AgendaAdvancedWithOther
  {- Ultimatum of Spoilage: "if an Item asset from the Expedition encounter set
  is ever defeated or discarded, it cannot be chosen during setup for the
  remainder of the campaign." Recorded in the campaign log, which is what
  'getAvailableExpeditionItems' filters on. -}
  Discarded (AssetTarget _) _ card ->
    whenM (hasUltimatum UltimatumOfSpoilage) $ spoilExpeditionItem (toCardDef card)
  {- Ultimatum of Death, the Specter's new Forced. 'cancelEnemyDefeat' drops the
  whole queued defeat chain, victory display included, so what is left is the
  heal, the exhaust and the upkeep lock. -}
  UseCardAbility _ (UltimatumOrBoonSource (Ultimatum UltimatumOfDeath)) 1 (defeatedEnemy -> eid) _ ->
    runQueueT do
      let source = UltimatumOrBoonSource (Ultimatum UltimatumOfDeath)
      cancelEnemyDefeat eid
      healAllDamage source eid
      exhaustWith source eid
      push =<< nextPhaseModifier #upkeep source eid DoesNotReadyDuringUpkeep
  {- Boon of The Dreamer. The plain 'AdvanceToAgenda' is what removes the doom:
  the agenda runner prefixes a 'RemoveAllDoomFromPlay' to it. -}
  UseCardAbility _ (UltimatumOrBoonSource (Boon BoonOfTheDreamer)) 1 _ _ -> runQueueT do
    advanceToAgendaA (fromUltimatumOrBoon (Boon BoonOfTheDreamer)) Agendas.chaosIncarnate
  {- Ultimatum of Death: "after setup, immediately advance Agenda 1a to Specter
  of Death and spawn it at your starting location, exhausted." The agenda's own
  side-B handler draws the Specter, which spawns at position (0,0) by its own
  printed text, so only the advance and the exhaust are needed here. -}
  EnemySpawn details -> whenM (hasUltimatum UltimatumOfDeath) do
    code <- fieldMap EnemyCard toCardCode details.enemy
    when (code == "03241b") $ runQueueT do
      exhaustWith (UltimatumOrBoonSource (Ultimatum UltimatumOfDeath)) details.enemy
  {- Ultimatum of Ambuscade. The card is only looked at -- it is drawn or
  discarded by its own message, so nothing has to be put back. -}
  UseCardAbility iid (UltimatumOrBoonSource (Ultimatum UltimatumOfAmbuscade)) 1 _ _ -> runQueueT do
    let source = UltimatumOrBoonSource (Ultimatum UltimatumOfAmbuscade)
    peeked <- headMay . unDeck <$> getEncounterDeck
    for_ peeked \card -> focusCards [toCard card] do
      if toCard card `cardMatch` Matcher.CardWithType EnemyType
        then drawEncounterCard iid source
        else discardTopOfEncounterDeck iid source 1
  _ -> pure ()

{- | Boon of the Morrígan: instead of adding a random basic weakness, draw
three, return one of the player's choice to the collection, and add one at
random from the remaining two. The random pick is pre-sampled per branch so
the choice resolves deterministically (undo/replay safe).

Each drawn weakness is presented as a self-describing 'CardLabel' rather than a
'FocusCards'-backed target. 'FocusCards' writes a single global focus list, so
in multiplayer two investigators resolving this choice inside the same
ChooseDecks window (every InitDeck runs there) would clobber each other's focus
and one player could never resolve theirs — ending up with no weakness.
'CardLabel' carries the card in the choice itself (the client renders it from
the card database), so the choices are independent per player.

'AddCampaignCardToDeck' is handled in both campaign and standalone modes, so
one message shape covers both @InitDeck@ call sites.
-}
morriganWeaknessMessages
  :: (HasGame m, MonadRandom m)
  => InvestigatorId
  -> m Card
  -> m [Message]
morriganWeaknessMessages iid drawWeakness = do
  cards <- distinctWeaknesses (50 :: Int) 3 []
  player <- getPlayer iid
  choices <- for cards \returned -> do
    kept <- case nonEmpty (filter ((/= toCardId returned) . toCardId) cards) of
      Nothing -> error "morrigan: fewer than two remaining weaknesses"
      Just remaining -> sample remaining
    pure $ CardLabel (toCardCode returned) False [AddCampaignCardToDeck iid ShuffleIn kept]
  pure
    [Ask player $ QuestionLabel "$label.ultimatumsAndBoons.returnWeakness" Nothing $ ChooseOne choices]
 where
  -- The basic weakness pool is far larger than 3; the fuel only guards
  -- against a pathological sampler.
  --
  -- Distinctness is by canonical card code, not 'CardDef' equality: two draws can be
  -- different printings of the same weakness (Mob Enforcer is 01101 in Core and 01601 in
  -- Revised Core), and those 'CardDef's are not equal, so the player would be offered the
  -- same card twice (#5264).
  distinctWeaknesses _ (0 :: Int) acc = pure (reverse acc)
  distinctWeaknesses 0 _ acc = pure (reverse acc)
  distinctWeaknesses fuel n acc = do
    card <- drawWeakness
    if canonicalCardCode (toCardDef card) `elem` map (canonicalCardCode . toCardDef) acc
      then distinctWeaknesses (fuel - 1) n acc
      else distinctWeaknesses (fuel - 1) (n - 1) (card : acc)

{- | Boon of the Ancients: each investigator begins the campaign with 5
additional experience. Granted alongside deck initialization (like
cdGrantedXp), immediately spendable at the first upgrade.
-}
ancientsStartingXpMessages :: InvestigatorId -> [Message]
ancientsStartingXpMessages iid =
  [ ReportXp
      ( XpBreakdown
          [InvestigatorGainXp iid $ XpDetail XpFromCardEffect "$xp.boonOfTheAncients" 5]
      )
  , GainXP iid (UltimatumOrBoonSource (Boon BoonOfTheAncients)) 5
  ]

{- | Ultimatum of Annoyance: "when the campaign begins, shuffle 3 random cards
from the Tekeli-li encounter set into each investigator's deck."

The set is read out of the player pool rather than with 'gatherEncounterSet',
which drops weaknesses -- and every Tekeli-li card is one. Edge of the Earth's
own 'gatherTekelili' does the same thing minus the cards already dealt out, but
importing it here would close a module cycle (its helpers reach Scenario.Setup,
which reaches this module through Scenario.Runner), and at campaign start
nothing has been dealt yet.
-}
annoyanceTekeliliMessages :: CardGen m => InvestigatorId -> m [Message]
annoyanceTekeliliMessages iid = do
  defs <- take 3 <$> shuffleM tekeliliDefs
  cards <- traverse genCard defs
  pure [AddCampaignCardToDeck iid ShuffleIn card | card <- cards]
 where
  tekeliliDefs =
    concatMap (\def -> replicate (fromMaybe 0 (cdEncounterSetQuantity def)) def)
      $ filter ((== Just Tekelili) . cdEncounterSet)
      $ toList allPlayerCards

-- Ultimatum of the Brass Crown's tally, per investigator, reset each scenario.
-- Deliberately not the Carcosa achievements module's counter: that one is a
-- campaign-lifetime total and only runs in Return to Carcosa.
spokenHasturKey :: InvestigatorId -> Text
spokenHasturKey iid = "carcosaSpokenHasturSinceScenario:" <> tshow iid

spokenHasturCount :: HasGame m => InvestigatorId -> m Int
spokenHasturCount iid = fromMaybe 0 <$> stored (spokenHasturKey iid)

bumpSpokenHastur :: HasQueue Message m => InvestigatorId -> Int -> m ()
bumpSpokenHastur iid n =
  push $ Priority $ IncrementGlobal CampaignTarget (Key.fromText $ spokenHasturKey iid) n

setSpokenHastur :: HasQueue Message m => InvestigatorId -> Int -> m ()
setSpokenHastur iid n =
  push $ Priority $ SetGlobal CampaignTarget (Key.fromText $ spokenHasturKey iid) (toJSON n)

{- | Record an Expedition Item as lost for the rest of the campaign (Ultimatum
of Spoilage). Anything else defeated or discarded is ignored.
-}
spoilExpeditionItem :: HasQueue Message m => CardDef -> m ()
spoilExpeditionItem def =
  when (def `elem` expeditionItems) $ push $ recordSetInsert SpoiledExpeditionItems [toCardCode def]

-- | The five Undimensioned and Unseen locations that are not Dunwich Village.
broodLocationTitles :: [Text]
broodLocationTitles =
  [ "Cold Spring Glen"
  , "Ten-Acre Meadow"
  , "Blasted Heath"
  , "Whateley Ruins"
  , "Devil's Hop Yard"
  ]
