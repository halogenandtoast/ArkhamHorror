{-# OPTIONS_GHC -Wno-unused-imports #-}

module Arkham.Message.Lifted.Card where


import Arkham.Helpers.FetchCard as X

import Arkham.Ability
import Arkham.Act.Sequence qualified as Act
import Arkham.Act.Types (ActAttrs (actDeckId))
import Arkham.Action (Action)
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Agenda.Types (AgendaAttrs (agendaDeckId))
import Arkham.Aspect (IsAspect (..))
import Arkham.Aspect qualified as Msg
import Arkham.Asset.Types (AssetAttrs)
import Arkham.Asset.Types qualified as Field
import Arkham.Asset.Uses (UseType)
import Arkham.Attack
import Arkham.Calculation
import Arkham.CampaignStep hiding (continue)
import Arkham.CampaignStep qualified as CS
import Arkham.Campaigns.TheScarletKeys.Key.Id
import Arkham.Capability
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue hiding (insertAfterMatching)
import Arkham.Classes.HasQueue as X (runQueueT)
import Arkham.Classes.Query
import Arkham.DamageEffect
import Arkham.Deck (IsDeck (..))
import Arkham.Deck qualified as Deck
import Arkham.Discover as X (IsInvestigate (..))
import Arkham.Discover qualified as Msg
import Arkham.Draw.Types
import Arkham.Effect.Builder
import Arkham.Effect.Types (EffectBuilder (effectBuilderEffectId), Field (..))
import Arkham.Effect.Window
import Arkham.EffectMetadata (EffectMetadata)
import Arkham.EncounterSet
import Arkham.Enemy.Creation
import Arkham.Enemy.Helpers qualified as Msg
import Arkham.Enemy.Types (Field (..))
import Arkham.Evade
import Arkham.Evade qualified as Evade
import Arkham.Exhaust qualified as Exhaust
import Arkham.Fight
import Arkham.Fight qualified as Fight
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Act
import Arkham.Helpers.Agenda
import Arkham.Helpers.Campaign
import Arkham.Helpers.Campaign qualified as Msg
import Arkham.Helpers.Card (getCardEntityTarget)
import Arkham.Helpers.ChaosToken qualified as Msg
import Arkham.Helpers.Effect qualified as Msg
import Arkham.Helpers.Enemy qualified as Msg
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Investigator (
  canHaveDamageHealed,
  canHaveHorrorHealed,
  getCanDiscoverClues,
 )
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Message.Discard qualified as HandDiscard
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.Query
import Arkham.Helpers.Ref (sourceToTarget)
import Arkham.Helpers.Scenario (getEncounterDeckKey, getInResolution, getIsStandalone)
import Arkham.Helpers.Shuffle
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.UI qualified as Msg
import Arkham.Helpers.Window qualified as Msg
import Arkham.Helpers.Xp
import Arkham.History
import Arkham.I18n
import Arkham.Id
import Arkham.Investigate
import Arkham.Investigate qualified as Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Layout
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..), Location)
import Arkham.Matcher hiding (PerformAction)
import Arkham.Message hiding (story)
import Arkham.Message as X (AndThen (..), getChoiceAmount, optionWhenExists, preOriginalOption)
import Arkham.Message.Lifted.Queue as X
import Arkham.Message.Lifted.Base
import Arkham.Modifier
import Arkham.Name
import Arkham.Phase (Phase)
import Arkham.Placement
import Arkham.Prelude hiding (pred)
import Arkham.Projection
import Arkham.Query
import Arkham.Queue
import Arkham.RequestedChaosTokenStrategy
import Arkham.Scenario.Deck
import Arkham.Search qualified as Search
import Arkham.SkillTest.Step
import Arkham.SkillType
import Arkham.SkillType qualified as SkillType
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Arkham.Token
import Arkham.Tracing
import Arkham.Trait (Trait)
import Arkham.Window (Window (..), WindowType, defaultWindows)
import Arkham.Window qualified as Window
import Arkham.Xp
import Control.Monad.State.Strict (MonadState, StateT, execStateT, get, put)
import Control.Monad.Trans.Class
import Data.Aeson.Key qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Typeable

setEncounterDeck :: ReverseQueue m => Deck EncounterCard -> m ()
setEncounterDeck = push . SetEncounterDeck

setAsideCards :: ReverseQueue m => [CardDef] -> m ()
setAsideCards = genCards >=> push . Msg.SetAsideCards

setCardAside :: (ReverseQueue m, IsCard a) => a -> m ()
setCardAside (toCard -> c) = do
  obtainCard c
  push $ Msg.SetAsideCards [c]

shuffleSetAsideEncounterSet :: ReverseQueue m => EncounterSet -> m ()
shuffleSetAsideEncounterSet eset = do
  cards <- getSetAsideCardsMatching (fromSets [eset])
  push $ ShuffleCardsIntoDeck Deck.EncounterDeck cards

shuffleEncounterDiscardBackIn :: ReverseQueue m => m ()
shuffleEncounterDiscardBackIn = push ShuffleEncounterDiscardBackIn

returnToHand :: (Targetable a, ReverseQueue m) => InvestigatorId -> a -> m ()
returnToHand iid = push . ReturnToHand iid . toTarget

addToVictory_ :: (ReverseQueue m, Targetable target) => target -> m ()
addToVictory_ = push . AddToVictory Nothing . toTarget

keepCardCache :: ReverseQueue m => m ()
keepCardCache = setupModifier GameSource GameTarget Persist

putCardIntoPlay :: (ReverseQueue m, IsCard card) => InvestigatorId -> card -> m ()
putCardIntoPlay iid card = push $ Msg.putCardIntoPlayWithAdditionalCosts iid card

putCampaignCardIntoPlay :: (ReverseQueue m, HasCardDef def) => InvestigatorId -> def -> m ()
putCampaignCardIntoPlay iid def = push $ PutCampaignCardIntoPlay iid (toCardDef def)

drawEncounterCard :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
drawEncounterCard i source = drawEncounterCards i source 1

drawEncounterCards :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
drawEncounterCards i source n = whenM (can.target.encounterDeck i) $ push $ Msg.drawEncounterCards i source n

shuffleIntoDeck :: (ReverseQueue m, IsDeck deck, Targetable target) => deck -> target -> m ()
shuffleIntoDeck deck target = whenCanShuffleIn deck (toTarget target) do
  push $ Msg.shuffleIntoDeck deck target

shuffleDeck :: (ReverseQueue m, IsDeck deck, HasCallStack) => deck -> m ()
shuffleDeck deck = guardPlayerDeckIsNotEmpty deck $ push $ ShuffleDeck (toDeck deck)

don'tAddToVictory :: (MonadTrans t, HasQueue Message m) => EnemyId -> t m ()
don'tAddToVictory eid = matchingDon't \case
  AddToVictory _ target -> isTarget eid target
  _ -> False

ifCardExists :: ReverseQueue m => ExtendedCardMatcher -> QueueT Message m () -> m ()
ifCardExists matcher body = do
  msgs <- capture body
  push $ IfCardExists matcher msgs

exile :: (ReverseQueue m, Targetable target) => target -> m ()
exile (toTarget -> target) = push $ Msg.Exile target

obtainCard :: (FetchCard a, ReverseQueue m) => a -> m ()
obtainCard a =
  fetchCardMaybe a >>= traverse_ \card -> do
    filterInbox \case
      Arkham.Message.Discarded _ _ discarded -> discarded.id == toCardId card
      _ -> False
    -- A committed skill keeps a Skill entity (placement Limbo) that is tracked
    -- separately from the skill test's committed-cards list. ObtainCard only
    -- clears the latter, so the entity would still be discarded when the test
    -- ends (e.g. War of the Outer Gods placing a committed skill as a swarm
    -- card). Remove that committed entity quietly here.
    selectOne (SkillWithCardId (toCardId card) <> SkillWithPlacement Limbo)
      >>= traverse_ (push . RemoveSkill)
    push $ ObtainCard $ toCardId card

removeCardFromGame :: (ReverseQueue m, IsCard card) => card -> m ()
removeCardFromGame card = do
  obtainCard $ toCard card
  push $ RemovedFromGame (toCard card)

playCardPayingCost :: ReverseQueue m => InvestigatorId -> Card -> m ()
playCardPayingCost iid card = do
  withTimings (Window.PlayCard iid $ Window.CardPlay card False) $ payCardCost iid card

payCardCost :: (ReverseQueue m, IsCard card) => InvestigatorId -> card -> m ()
payCardCost iid card = push $ Msg.PayCardCost iid (toCard card) (defaultWindows iid)

playCardPayingCostWithWindows :: ReverseQueue m => InvestigatorId -> Card -> [Window] -> m ()
playCardPayingCostWithWindows iid card ws = do
  withTimings (Window.PlayCard iid $ Window.CardPlay card False) $ payCardCostWithWindows iid card ws

payCardCostWithWindows :: ReverseQueue m => InvestigatorId -> Card -> [Window] -> m ()
payCardCostWithWindows iid card ws = push $ Msg.PayCardCost iid card ws

removeFromGame :: (ReverseQueue m, Targetable target) => target -> m ()
removeFromGame = push . Msg.RemoveFromGame . toTarget

changeDrawnBy :: (MonadTrans t, HasQueue Message m) => InvestigatorId -> InvestigatorId -> t m ()
changeDrawnBy drawer newDrawer =
  lift $ replaceAllMessagesMatching
    \case
      Revelation me _ -> me == drawer
      Do (InvestigatorDrewEncounterCard me _) -> me == drawer
      Do (InvestigatorDrewEncounterCardFrom me _ _) -> me == drawer
      InvestigatorDrawEnemy me _ -> me == drawer
      CheckWindows ws -> any (isDrawCard . Window.windowType) ws
      Do (CheckWindows ws) -> any (isDrawCard . Window.windowType) ws
      _ -> False
    \case
      Revelation _ source' -> [Revelation newDrawer source']
      InvestigatorDrawEnemy _ eid -> [InvestigatorDrawEnemy newDrawer eid]
      Do (InvestigatorDrewEncounterCard _ c) -> [Do (InvestigatorDrewEncounterCard newDrawer c)]
      Do (InvestigatorDrewEncounterCardFrom _ c frm) -> [Do (InvestigatorDrewEncounterCardFrom newDrawer c frm)]
      CheckWindows ws -> [CheckWindows $ map changeWindow ws]
      Do (CheckWindows ws) -> [Do (CheckWindows $ map changeWindow ws)]
      _ -> error "wrong message found"
 where
  isDrawCard = \case
    Window.DrawCard who _ _ -> who == drawer
    _ -> False
  changeWindow = \case
    Window.Window t (Window.DrawCard who c f) batchId | who == drawer -> Window.Window t (Window.DrawCard newDrawer c f) batchId
    other -> other

attach :: (HasQueue Message m, Attachable a, Targetable target) => a -> target -> m ()
attach a = push . toAttach a

shuffleSetAsideEncounterSetIntoEncounterDeck :: ReverseQueue m => EncounterSet -> m ()
shuffleSetAsideEncounterSetIntoEncounterDeck =
  getSetAsideEncounterSet
    >=> shuffleCardsIntoDeck Deck.EncounterDeck
    . filter (and . sequence [not . cdDoubleSided, isNothing . cdOtherSide] . toCardDef)

shuffleSetAsideIntoEncounterDeck :: (ReverseQueue m, IsCardMatcher matcher) => matcher -> m ()
shuffleSetAsideIntoEncounterDeck matcher = do
  cards <- getSetAsideCardsMatching (toCardMatcher matcher)
  push $ ShuffleCardsIntoDeck Deck.EncounterDeck cards

shuffleScenarioDeckIntoEncounterDeck :: ReverseQueue m => ScenarioDeckKey -> m ()
shuffleScenarioDeckIntoEncounterDeck = push . ShuffleScenarioDeckIntoEncounterDeck

setScenarioDeck :: ReverseQueue m => ScenarioDeckKey -> [Card] -> m ()
setScenarioDeck key cards = push $ Msg.SetScenarioDeck key cards

removeScenarioDeck :: ReverseQueue m => ScenarioDeckKey -> m ()
removeScenarioDeck key = setScenarioDeck key []

drawCardFrom :: (IsDeck deck, IsCard card, ReverseQueue m) => InvestigatorId -> deck -> card -> m ()
drawCardFrom iid deck (toCard -> card) = do
  obtainCard $ toCard card
  case card of
    EncounterCard ec -> push $ InvestigatorDrewEncounterCardFrom iid ec (Just $ toDeck deck)
    PlayerCard pc -> push $ InvestigatorDrewPlayerCardFrom iid pc (Just $ toDeck deck)
    VengeanceCard vc -> Arkham.Message.Lifted.Card.drawCardFrom iid deck vc

drawCard :: (ReverseQueue m, FetchCard card) => InvestigatorId -> card -> m ()
drawCard iid card = do
  c <- fetchCard card
  obtainCard c
  case c of
    EncounterCard ec -> push $ InvestigatorDrewEncounterCard iid ec
    PlayerCard pc -> push $ InvestigatorDrewPlayerCardFrom iid pc Nothing
    VengeanceCard vc -> Arkham.Message.Lifted.Card.drawCard iid vc

discard :: (IsCard card, ReverseQueue m) => card -> m ()
discard card = push $ DiscardedCard (toCardId card)

shuffleBackIntoEncounterDeck :: (ReverseQueue m, Targetable target) => target -> m ()
shuffleBackIntoEncounterDeck target = push $ ShuffleBackIntoEncounterDeck GameSource (toTarget target)

commitCard :: (ReverseQueue m, IsCard card) => InvestigatorId -> card -> m ()
commitCard iid card = push $ SkillTestCommitCard iid (toCard card)

revealCard :: (IsCard card, ReverseQueue m, HasGameLogger m) => card -> m ()
revealCard card = do
  sendReveal (toJSON $ toCard card)
  push $ RevealCard (toCardId card)

addToEncounterDiscard :: (ReverseQueue m, IsCard a, Element xs ~ a, MonoFoldable xs) => xs -> m ()
addToEncounterDiscard = traverse_ (push . AddToEncounterDiscard) . mapMaybe (preview _EncounterCard . toCard) . otoList

allRandomDiscard :: (ReverseQueue m, Sourceable source) => source -> CardMatcher -> m ()
allRandomDiscard source matcher = push $ AllRandomDiscard (toSource source) matcher

allDrawEncounterCard :: ReverseQueue m => m ()
allDrawEncounterCard = push Msg.AllDrawEncounterCard

class Attachable a where
  toAttach :: Targetable target => a -> target -> Message

instance Attachable AssetAttrs where
  toAttach attrs target = AttachAsset (asId attrs) (toTarget target)

instance Attachable EventId where
  toAttach eid target = AttachEvent eid (toTarget target)
