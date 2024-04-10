module Arkham.Investigator.Cards.GloriaGoldberg (gloriaGoldberg, GloriaGoldberg (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getPlayer)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorCardsUnderneath))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message qualified as Msg
import Arkham.Modifier
import Arkham.Projection
import Arkham.Strategy
import Arkham.Trait (Trait (Elite))

newtype GloriaGoldberg = GloriaGoldberg InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gloriaGoldberg :: InvestigatorCard GloriaGoldberg
gloriaGoldberg =
  investigator GloriaGoldberg Cards.gloriaGoldberg
    $ Stats {health = 5, sanity = 9, willpower = 5, intellect = 4, combat = 2, agility = 1}

instance HasAbilities GloriaGoldberg where
  getAbilities (GloriaGoldberg attrs) =
    [restrictedAbility attrs 1 Self $ freeReaction $ LookedAtDeck #when You EncounterDeck]

instance HasChaosTokenValue GloriaGoldberg where
  getChaosTokenValue iid ElderSign (GloriaGoldberg attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign NoModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage GloriaGoldberg where
  runMessage msg i@(GloriaGoldberg attrs) = runQueueT $ case msg of
    SetupInvestigator iid | iid == attrs.id -> do
      attrs' <- lift (runMessage msg attrs)
      pure . GloriaGoldberg $ attrs' & setMeta False
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      searchModifier (attrs.ability 1) iid (LookAtDepth 1)
      pure . GloriaGoldberg $ attrs & setMeta True
    EndSearch {} -> do
      attrs' <- lift (runMessage msg attrs)
      pure . GloriaGoldberg $ attrs' & setMeta False
    ElderSignEffect iid | attrs `is` iid -> do
      lookAt iid ElderSign EncounterDeckTarget [(FromTopOfDeck 1, PutBack)] AnyCard ReturnCards
      pure i
    FoundCards cards -> do
      when (toResult attrs.meta) do
        let nonEliteCards = filter (`cardMatch` NotCard (CardWithTrait Elite)) (concat $ toList cards)
        let iid = attrs.id
        unless (null nonEliteCards) do
          player <- getPlayer iid
          cardsUnderneathCount <- fieldMap InvestigatorCardsUnderneath length iid
          chooseOne
            iid
            [ TargetLabel
              (CardIdTarget $ toCardId card)
              [ Msg.chooseOne
                  player
                  $ [ Label "Discard it" [AddToEncounterDiscard ec]
                    , Label "Put it on top of the encounter deck" [PutCardOnTopOfDeck iid Deck.EncounterDeck card]
                    ]
                  <> [ Label
                      "Place it beneath Gloria Goldberg (max 3 cards beneath her)"
                      [ObtainCard card, PlaceUnderneath (toTarget iid) [card]]
                     | cardsUnderneathCount < 3
                     ]
              ]
            | card@(EncounterCard ec) <- nonEliteCards
            ]
      pure . GloriaGoldberg $ attrs & setMeta False
    _ -> GloriaGoldberg <$> lift (runMessage msg attrs)
