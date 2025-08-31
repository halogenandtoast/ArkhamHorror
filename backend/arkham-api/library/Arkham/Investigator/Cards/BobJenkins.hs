module Arkham.Investigator.Cards.BobJenkins (bobJenkins) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Capability
import Arkham.Card
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf, withModifiersOf)
import Arkham.Helpers.Playable (getPlayableCardsMatch)
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (defaultWindows)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Map.Strict qualified as Map

newtype BobJenkins = BobJenkins InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

bobJenkins :: InvestigatorCard BobJenkins
bobJenkins =
  investigator
    BobJenkins
    Cards.bobJenkins
    Stats {health = 6, sanity = 8, willpower = 2, intellect = 4, combat = 3, agility = 3}

parseRevealedCards :: Value -> Parser (Map InvestigatorId [CardId])
parseRevealedCards = withObject "RevealedCards" \o -> o .:? "revealedCards" .!= mempty

getRevealedCards :: Value -> Map InvestigatorId [CardId]
getRevealedCards = fromMaybe mempty . parseMaybe parseRevealedCards

instance HasModifiersFor BobJenkins where
  getModifiersFor (BobJenkins a) = modifySelf a [GiveAdditionalAction $ bobJenkinsAction a]

instance HasAbilities BobJenkins where
  getAbilities (BobJenkins attrs) =
    [ selfAbility attrs 1 (PlayableCardExists (UnpaidCost NoAction) matchCards) actionAbility
    | BobJenkinsAction `notElem` map additionalActionType attrs.usedAdditionalActions
    ]
   where
    matchCards = basic (#asset <> #item) <> InHandOf ForPlay (affectsColocatedMatch You)

bobJenkinsAction :: Sourceable source => source -> AdditionalAction
bobJenkinsAction source = AdditionalAction "Bob Jenkins" (toSource source) BobJenkinsAction

instance HasChaosTokenValue BobJenkins where
  getChaosTokenValue iid ElderSign (BobJenkins attrs) | iid == attrs.id = do
    n <- selectCount $ assetControlledBy iid <> #item
    pure $ ChaosTokenValue ElderSign (PositiveModifier n)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage BobJenkins where
  runMessage msg i@(BobJenkins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let windows' = defaultWindows iid
      investigators <- select (affectsColocated iid)
      playableCards <- concatForM investigators \iid' -> do
        cards <- select $ inHandOf ForPlay iid' <> basic (#asset <> #item)
        withModifiersOf iid' attrs [CanSpendResourcesOnCardFromInvestigator (be iid) AnyCard] do
          withModifiersOf iid attrs (PlayableCardOf iid' <$> cards) do
            getPlayableCardsMatch attrs iid (UnpaidCost NoAction) windows' (card_ $ #asset <> #item)

      chooseTargetM iid playableCards $ handleTarget iid (attrs.ability 1)
      pure $ BobJenkins $ attrs & (usedAdditionalActionsL %~ (bobJenkinsAction attrs :))
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      c <- getCard cid
      for_ c.owner \owner -> do
        cardResolutionModifier c (attrs.ability 1) owner
          $ CanSpendResourcesOnCardFromInvestigator (be iid) AnyCard
        cardResolutionModifier c (attrs.ability 1) attrs (PlayableCardOf owner c)
        playCardPayingCost iid c
      pure i
    ResetGame -> BobJenkins <$> liftRunMessage msg (attrs & setMeta Null)
    _ -> do
      let revealedCards = getRevealedCards attrs.meta
      -- remove any cards no longer in hand
      let
        addCards revealedCards' iid = do
          handCards <- map toCardId . filterCards (card_ $ #asset <> #item) <$> field InvestigatorHand iid
          pure $ Map.insert iid handCards revealedCards'

      iids <- select $ affectsOthers $ colocatedWith attrs.id <> not_ (be attrs.id) <> can.reveal.cards
      revealedCards' <- foldM addCards revealedCards iids
      BobJenkins <$> liftRunMessage msg (attrs & setMeta (object ["revealedCards" .= revealedCards']))
