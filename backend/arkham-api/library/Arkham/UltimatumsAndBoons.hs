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
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.ChaosToken.Types (ChaosTokenFace (..))
import Arkham.Classes.HasGame
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasQueue
import Arkham.Classes.Query ((<=~>))
import Arkham.Deck qualified as Deck
import Arkham.Decklist.RandomBasicWeakness (
  RandomBasicWeaknessContext (..),
  sampleRandomBasicWeakness,
 )
import Arkham.DefeatedBy
import Arkham.Event.Types (Event)
import Arkham.Game.Base
import Arkham.Game.Settings
import Arkham.Helpers.ChaosToken (cancelChaosToken)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario (getIsStandalone)
import Arkham.Helpers.Window (checkWindows)
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
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Arkham.Trait (Trait (Ally))
import Arkham.UltimatumsAndBoons.Types
import Arkham.Window (mkAfter, revealedChaosTokens)
import Arkham.Window qualified as Window
import Arkham.Xp

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

{- | Marks an investigator who already used Boon of the Child this round.
Carried by a round-scoped window-modifier effect so it expires on its own.
-}
boonOfTheChildUsedMarker :: ModifierType
boonOfTheChildUsedMarker = MetaModifier "usedBoonOfTheChild"

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
      UltimatumOfTheScream -> do
        screamed <- settingsScreamedAllies . gameSettings <$> getGame
        unless (null screamed) do
          modifySelect
            source
            Matcher.Anyone
            [CannotPlay $ Matcher.mapOneOf Matcher.CardWithCardCode (toList screamed)]
      _ -> pure ()

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
      BoonOfTheChild -> do
        modifySelectMaybe source Matcher.Anyone \iid -> do
          mods <- lift $ getModifiers iid
          guard $ boonOfTheChildUsedMarker `notElem` mods
          pure [CanPlayTopmostOfDiscard (Just EventType, [])]
        -- Bottom-deck instead of discard, computed from the event's own
        -- played-from zone: message-based effect creation would race the play
        -- chain (the scenario dispatches before entities, so pushed effects
        -- resolve only after the event has already discarded).
        modifySelectMaybe source Matcher.AnyEvent \eid -> do
          attrs <- lift $ getAttrs @Event eid
          guard attrs.playedFromDiscard
          pure [PlaceOnBottomOfDeckInsteadOfDiscard]
      _ -> pure ()

ultimatumOrBoonAbilities :: UltimatumOrBoon -> [Ability]
ultimatumOrBoonAbilities = \case
  Ultimatum {} -> []
  Boon b -> boonAbilities b

boonAbilities :: Boon -> [Ability]
boonAbilities b = case b of
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
  :: (HasGame m, HasQueue Message m, Tracing m, CardGen m)
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
        investigatorClass <- field InvestigatorClass iid
        playerCount <- getPlayerCount
        weakness <-
          genCard
            =<< sampleRandomBasicWeakness
              RandomBasicWeaknessContext
                { rbwInvestigatorClass = investigatorClass
                , rbwPlayerCount = playerCount
                , rbwDecklist = Nothing
                , rbwStandalone = False
                }
        push $ AddCampaignCardToDeck iid DoNotShuffleIn weakness
  -- Ultimatum of The Scream: a defeated unique non-story, non-weakness ally
  -- is removed from the game and banned for the rest of the campaign.
  When (AssetDefeated _ aid) -> do
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
  PlayCard iid card _ _ _ _ -> do
    whenM (hasBoon BoonOfTheChild) do
      mods <- getModifiers iid
      unless (boonOfTheChildUsedMarker `elem` mods) do
        discard' <- field InvestigatorDiscard iid
        -- "topmost event": the first event from the top, whatever sits above
        -- it (matches CanPlayTopmostOfDiscard's filtered-then-head semantics).
        case find (`cardMatch` Matcher.CardWithType EventType) discard' of
          Just topmostEvent | toCardId topmostEvent == toCardId card -> do
            -- Attributed to this boon even if another effect also allows
            -- discard plays. Only the once-per-round marker is pushed here;
            -- the bottom-decking is a computed modifier (HasModifiersFor) on
            -- events played from the discard, since a pushed effect would
            -- resolve after the event has already discarded.
            marker <-
              roundModifier
                (UltimatumOrBoonSource (Boon BoonOfTheChild))
                (InvestigatorTarget iid)
                boonOfTheChildUsedMarker
            push marker
          _ -> pure ()
  _ -> pure ()

{- | Boon of the Morrígan: instead of adding a random basic weakness, draw
three, return one of the player's choice to the collection, and add one at
random from the remaining two. The random pick is pre-sampled per branch so
the choice resolves deterministically (undo/replay safe).

'AddCampaignCardToDeck' is handled in both campaign and standalone modes, so
one message shape covers both @InitDeck@ call sites.
-}
morriganWeaknessMessages
  :: (HasGame m, MonadRandom m, Tracing m)
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
    pure $ targetLabel (toCardId returned) [AddCampaignCardToDeck iid ShuffleIn kept]
  pure
    [ FocusCards cards
    , Ask player $ QuestionLabel "$label.ultimatumsAndBoons.returnWeakness" Nothing $ ChooseOne choices
    , UnfocusCards
    ]
 where
  -- The basic weakness pool is far larger than 3; the fuel only guards
  -- against a pathological sampler.
  distinctWeaknesses _ (0 :: Int) acc = pure (reverse acc)
  distinctWeaknesses 0 _ acc = pure (reverse acc)
  distinctWeaknesses fuel n acc = do
    card <- drawWeakness
    if toCardDef card `elem` map toCardDef acc
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
