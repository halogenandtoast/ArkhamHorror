module Arkham.Asset.Assets.DeckOfPossibilitiesTychokineticImplement (deckOfPossibilitiesTychokineticImplement) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Helpers.Campaign
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.PlayerCard
import Arkham.Scenarios.FortuneAndFolly.PlayingCard
import Arkham.Trait (Trait (Ally, Item))

newtype DeckOfPossibilitiesTychokineticImplement = DeckOfPossibilitiesTychokineticImplement AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deckOfPossibilitiesTychokineticImplement :: AssetCard DeckOfPossibilitiesTychokineticImplement
deckOfPossibilitiesTychokineticImplement = asset DeckOfPossibilitiesTychokineticImplement Cards.deckOfPossibilitiesTychokineticImplement

instance HasAbilities DeckOfPossibilitiesTychokineticImplement where
  getAbilities (DeckOfPossibilitiesTychokineticImplement a) =
    [playerLimit PerGame $ controlled a 1 criteria actionAbility]
   where
    criteria =
      if isJust (getMetaKeyMaybe @[Card] "extraDeck" a)
        then NoRestriction
        else Never

instance RunMessage DeckOfPossibilitiesTychokineticImplement where
  runMessage msg a@(DeckOfPossibilitiesTychokineticImplement attrs) = runQueueT $ case msg of
    CardEnteredPlay _iid card | card.id == attrs.cardId -> do
      deck <- shuffle . fromMaybe [] =<< stored "deckOfPossibilities"
      pure . DeckOfPossibilitiesTychokineticImplement $ attrs & setMetaKey @[Card] "extraDeck" deck
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let filterIt t def =
            (def.kind == AssetType)
              && (t `elem` def.printedTraits)
              && isJust def.level
              && not def.permanent
              && isJust def.cost
      let cards = mapMaybe lookupCardDef $ toList allPlayerCards
      for_ (getMetaKeyMaybe @[Card] "extraDeck" attrs >>= nonEmpty) \(c :| rest) -> do
        focusCards [c] $ continue_ iid
        doStep 1 msg
        for_ (toPlayingCardPure c) \pc -> do
          case (pc.rank, pc.suit) of
            (Jack, Hearts) -> gainActions iid (attrs.ability 1) 3
            (Jack, Diamonds) -> gainResources iid (attrs.ability 1) 10
            (Jack, Clubs) -> chooseNM iid 2 do
              for_ [#willpower, #intellect, #combat, #agility] \skill ->
                skillLabeled skill $ gameModifier (attrs.ability 1) iid (SkillModifier skill 1)
            (Queen, Diamonds) -> do
              chooseOneM iid do
                for_ (filter (filterIt Ally) cards) \def -> do
                  cardLabeled def $ handleTarget iid (attrs.ability 1) (CardCodeTarget def.cardCode)
            (Queen, Spades) -> do
              chooseOneM iid do
                for_ (filter (filterIt Item) cards) \def -> do
                  cardLabeled def $ handleTarget iid (attrs.ability 1) (CardCodeTarget def.cardCode)
            (King, Hearts) -> healAllDamageAndHorror (attrs.ability 1) iid
            (King, Clubs) -> do
              enemies <- select $ InPlayEnemy NonEliteEnemy
              treacheries <- select InPlayTreachery
              chooseOneM iid do
                targets enemies removeFromGame
                targets treacheries removeFromGame
            (Ace, Spades) -> gameModifier (attrs.ability 1) iid (ForcedChaosTokenChange ElderSign [AutoFail])
            _ -> pure ()
        setGlobal CampaignTarget "deckOfPossibilities" (toJSON rest)
      pure $ DeckOfPossibilitiesTychokineticImplement $ attrs & flippedL .~ True
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      pure $ DeckOfPossibilitiesTychokineticImplement $ attrs & flippedL .~ False
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- genCard def
        putCardIntoPlay iid card
      pure a
    _ -> DeckOfPossibilitiesTychokineticImplement <$> liftRunMessage msg attrs
