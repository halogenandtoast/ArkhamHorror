module Arkham.Investigator.Cards.DianaStanley (
  dianaStanley,
  DianaStanley (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Event.Cards qualified as Events
import Arkham.Event.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (AssetCard, EventCard, PlaceUnderneath, SkillCard)
import Arkham.Projection
import Arkham.Skill.Types (Field (..))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DianaStanley = DianaStanley InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

dianaStanley :: InvestigatorCard DianaStanley
dianaStanley =
  startsWithInHand [Events.darkInsight]
    $ investigator DianaStanley Cards.dianaStanley
    $ Stats {health = 7, sanity = 7, willpower = 1, intellect = 3, combat = 3, agility = 3}

instance HasModifiersFor DianaStanley where
  getModifiersFor (DianaStanley a) = do
    n <- fieldMap InvestigatorCardsUnderneath length (toId a)
    modifySelf a [SkillModifier #willpower n | n > 0]

instance HasAbilities DianaStanley where
  getAbilities (DianaStanley a) =
    [ playerLimit PerPhase
        $ restrictedAbility a 1 (Self <> fewerThan5CardBeneath)
        $ freeReaction
        $ CancelledOrIgnoredCardOrGameEffect (SourceOwnedBy You <> NotSource #investigator)
    ]
   where
    fewerThan5CardBeneath = if length a.cardsUnderneath < 5 then NoRestriction else Never

instance HasChaosTokenValue DianaStanley where
  getChaosTokenValue iid ElderSign (DianaStanley attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getCancelSource :: [Window] -> Source
getCancelSource [] = error "No window to cancel"
getCancelSource ((windowType -> Window.CancelledOrIgnoredCardOrGameEffect source) : _) = source
getCancelSource (_ : rest) = getCancelSource rest

instance RunMessage DianaStanley where
  runMessage msg i@(DianaStanley attrs) = case msg of
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      cardsUnderneath <- field InvestigatorCardsUnderneath iid
      player <- getPlayer iid
      when (notNull cardsUnderneath) $ do
        pushAll
          [ FocusCards cardsUnderneath
          , chooseOrRunOne player
              $ Label "Do not add any cards to your Hand" [UnfocusCards]
              : [targetLabel (toCardId c) [UnfocusCards, addToHand iid c] | c <- cardsUnderneath]
          ]
      pure i
    UseCardAbility iid (isSource attrs -> True) 1 (getCancelSource -> source) _ -> do
      let
        getSource = \case
          AssetSource aid -> (RemoveFromPlay (toSource aid),) <$> field AssetCard aid
          SkillSource sid -> (RemoveFromPlay (toSource sid),) <$> field SkillCard sid
          EventSource eid -> (RemoveFromPlay (toSource eid),) <$> field EventCard eid
          AbilitySource abilitySource _ -> getSource abilitySource
          CardIdSource cid -> do
            card <- getCard cid
            pure (RemovePlayerCardFromGame False card, card)
          _ -> error $ "Unhandled source for Diana Stanley: " <> show source
      (removeMsg, card) <- getSource source
      canLeavePlay <- case source of
        AssetSource aid -> elem aid <$> select AssetCanLeavePlayByNormalMeans
        _ -> pure $ not (cdPermanent $ toCardDef card)
      pushAll
        $ [removeMsg | canLeavePlay]
        <> (guard canLeavePlay *> [ObtainCard card.id, PlaceUnderneath (toTarget attrs) [card]])
        <> [ drawCards iid (attrs.ability 1) 1
           , TakeResources iid 1 (attrs.ability 1) False
           ]
      pure i
    _ -> DianaStanley <$> runMessage msg attrs
