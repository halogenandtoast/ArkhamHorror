module Arkham.Investigator.Cards.PatriceHathaway (
  patriceHathaway,
  PatriceHathaway (..),
)
where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Projection

newtype PatriceHathaway = PatriceHathaway InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock (Data)

patriceHathaway :: InvestigatorCard PatriceHathaway
patriceHathaway =
  investigator PatriceHathaway Cards.patriceHathaway
    $ Stats {health = 7, sanity = 7, willpower = 4, intellect = 2, combat = 2, agility = 2}

instance HasModifiersFor PatriceHathaway where
  getModifiersFor target (PatriceHathaway attrs) | attrs `is` target = do
    pure $ toModifiers attrs [HandSize (-3), AlternateUpkeepDraw (toTarget attrs)]
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue PatriceHathaway where
  getChaosTokenValue iid ElderSign (PatriceHathaway attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage PatriceHathaway where
  runMessage msg i@(PatriceHathaway attrs) = case msg of
    SendMessage (isTarget attrs -> True) AllDrawCardAndResource | not (attrs ^. defeatedL || attrs ^. resignedL) -> do
      attrs' <- takeUpkeepResources attrs
      hand <- field InvestigatorHand (toId attrs)
      let nonWeaknessCards = filter (`cardMatch` NonWeakness) hand
      player <- getPlayer (toId attrs)
      pushAll
        [ chooseOneAtATime player
            $ targetLabels nonWeaknessCards (only . DiscardCard (toId attrs) (toSource attrs) . toCardId)
        , DoStep 1 msg
        ]
      pure $ PatriceHathaway attrs'
    DoStep 1 (SendMessage (isTarget attrs -> True) AllDrawCardAndResource) | not (attrs ^. defeatedL || attrs ^. resignedL) -> do
      cards <- field InvestigatorHand (toId attrs)
      let numberToDraw = max 0 (5 - length cards)
      pushWhen (numberToDraw > 0) $ drawCards (toId attrs) ScenarioSource numberToDraw
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      insertAfterMatching [DoStep 1 msg] (== EndSkillTestWindow)
      pure i
    DoStep 1 msg'@(ResolveChaosToken _ ElderSign iid) | attrs `is` iid -> do
      canModifyDeck <- check attrs can.manipulate.deck
      canHaveCardsLeaveDiscard <- check attrs can.have.cards.leaveDiscard
      player <- getPlayer iid
      pushWhen (canModifyDeck && canHaveCardsLeaveDiscard && length attrs.discard > 1)
        $ chooseOrRunOne player
        $ [ Label "Shuffle all but 1 card from your discard pile into your deck" [DoStep 2 msg']
          , Label "Skip" []
          ]
      pure i
    DoStep 2 (ResolveChaosToken _ ElderSign iid) | attrs `is` iid -> do
      let discards = map toCard attrs.discard
      player <- getPlayer (toId attrs)
      pushAll
        [ FocusCards discards
        , questionLabel "Choose 1 card to leave in discard" player
            $ ChooseOne
              [ targetLabel card [UnfocusCards, ShuffleCardsIntoDeck (Deck.InvestigatorDeck (toId attrs)) rest]
              | (card, rest) <- eachWithRest discards
              ]
        ]

      pure i
    _ -> PatriceHathaway <$> runMessage msg attrs
