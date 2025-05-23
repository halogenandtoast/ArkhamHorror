module Arkham.Investigator.Cards.GloriaGoldberg (gloriaGoldberg) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Deck (withDeck)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorCardsUnderneath))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted qualified
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Strategy
import Arkham.Trait (Trait (Elite))
import Arkham.Treachery.Cards qualified as Treacheries

newtype GloriaGoldberg = GloriaGoldberg InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

gloriaGoldberg :: InvestigatorCard GloriaGoldberg
gloriaGoldberg =
  investigator GloriaGoldberg Cards.gloriaGoldberg
    $ Stats {health = 5, sanity = 9, willpower = 5, intellect = 4, combat = 2, agility = 1}

instance HasAbilities GloriaGoldberg where
  getAbilities (GloriaGoldberg attrs) =
    [restrictedAbility attrs 1 Self $ freeReaction $ LookedAtDeck #when You EncounterDeck]

instance HasChaosTokenValue GloriaGoldberg where
  getChaosTokenValue iid ElderSign (GloriaGoldberg attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage GloriaGoldberg where
  runMessage msg i@(GloriaGoldberg attrs) = runQueueT $ case msg of
    LoadDeck iid deck | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      let prophecies = filterCards (cardIs Treacheries.prophecyOfTheEnd) (unDeck deck)
      for_ prophecies \prophecy -> do
        let
          card =
            MkEncounterCard
              { ecId = prophecy.id
              , ecCardCode = prophecy.cardCode
              , ecOriginalCardCode = prophecy.cardCode
              , ecIsFlipped = Nothing
              , ecAddedPeril = False
              , ecOwner = Nothing
              }

        replaceCard prophecy.id (toCard card)
        setupModifier attrs ScenarioTarget $ StartsInEncounterDeck card
      pure
        $ GloriaGoldberg
        $ attrs'
        & deckL
        %~ withDeck (filterCards (not_ $ cardIs Treacheries.prophecyOfTheEnd))
    SetupInvestigator iid | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      pure . GloriaGoldberg $ attrs' & setMeta (object ["gloria" .= False])
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      searchModifier (attrs.ability 1) iid (LookAtDepth 1)
      pure . GloriaGoldberg $ attrs & setMeta (object ["gloria" .= True])
    EndSearch {} -> do
      attrs' <- liftRunMessage msg attrs
      pure . GloriaGoldberg $ attrs' & setMeta (object ["gloria" .= False])
    ElderSignEffect iid | attrs `is` iid -> do
      lookAt iid ElderSign EncounterDeckTarget [(FromTopOfDeck 1, PutBack)] #any ReturnCards
      pure i
    FoundCards cards -> do
      when (lookupMetaKeyWithDefault "gloria" False attrs) do
        let nonEliteCards = concatMap (filterCards (not_ (CardWithTrait Elite))) (toList cards)
        let propheciesOfTheEnd = concatMap (filterCards (cardIs Treacheries.prophecyOfTheEnd)) (toList cards)
        let iid = attrs.id
        cardsUnderneathCount <- fieldMap InvestigatorCardsUnderneath length iid
        if notNull propheciesOfTheEnd
          then do
            chooseOneAtATimeM iid do
              for_ propheciesOfTheEnd \prophecy -> do
                let ab =
                      buildAbility
                        ( SourceableWithCardCode
                            prophecy
                            (ProxySource (TreacherySource $ unsafeFromCardId prophecy.id) (toSource iid))
                        )
                        1
                        (forced AnyWindow)
                abilityLabeled iid ab nothing
          else unless (null nonEliteCards) do
            chooseOneM iid $ for_ nonEliteCards \card -> do
              targeting card $ chooseOneM iid do
                labeled "Discard it" (addToEncounterDiscard (only card))
                labeled "Put it on top of the encounter deck" $ putCardOnTopOfDeck iid Deck.EncounterDeck card
                when (cardsUnderneathCount < 3) do
                  labeled "Place it beneath Gloria Goldberg (max 3 cards beneath her)" do
                    obtainCard card
                    placeUnderneath iid [card]
      pure . GloriaGoldberg $ attrs & setMeta (object ["gloria" .= False])
    UseThisAbility iid (ProxySource (TreacherySource tid) (isSource attrs -> True)) 1 -> do
      mcard <- findCard ((== unsafeToCardId tid) . toCardId)
      case mcard of
        Just card | cardMatch card (cardIs Treacheries.prophecyOfTheEnd) -> do
          under <- field InvestigatorCardsUnderneath iid
          when (length under == 3) do
            sendShowUnder iid
            focusCards under do
              chooseTargetM iid (filterCards NonWeakness under) \undercard -> do
                obtainCard undercard
                addToEncounterDiscard (only undercard)
          obtainCard card
          placeUnderneath iid [card]
          doStep 1 msg
        _ -> pure ()
      pure i
    DoStep 1 (UseThisAbility iid (ProxySource (TreacherySource tid) (isSource attrs -> True)) 1) -> do
      mcard <- findCard ((== unsafeToCardId tid) . toCardId)
      case mcard of
        Just card | cardMatch card (cardIs Treacheries.prophecyOfTheEnd) -> do
          under <- filterCards (cardIs Treacheries.prophecyOfTheEnd) <$> field InvestigatorCardsUnderneath iid
          when (length under == 3) do
            sufferMentalTrauma iid 1
            Arkham.Message.Lifted.investigatorDefeated attrs iid
        _ -> pure ()
      pure i
    _ -> GloriaGoldberg <$> liftRunMessage msg attrs
