module Arkham.Treachery.Cards.Languor (languor) where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Window
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Languor = Languor TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

languor :: TreacheryCard Languor
languor = treachery Languor Cards.languor

instance HasAbilities Languor where
  getAbilities (Languor a) = case a.placement of
    InThreatArea _ ->
      [ restrictedAbility a 1 (InThreatAreaOf You) 
          $ forced 
          $ TurnBegins #after You
      , restrictedAbility a 2 (InThreatAreaOf You)
          $ ActionAbility mempty Nothing
          $ ActionCost 2
      ]
    _ -> []

instance RunMessage Languor where
  runMessage msg t@(Languor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Limit 1 per investigator: check if already in threat area
      alreadyHasOne <- selectAny $ 
        TreacheryInThreatAreaOf (InvestigatorWithId iid) 
        <> treacheryIs Cards.languor
      if alreadyHasOne
        then -- Already has one, discard this copy
          toDiscardBy iid (toSource attrs) attrs
        else -- Place in threat area
          placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Discard top card of deck
      discardTopOfDeckAndHandle iid (attrs.ability 1) 1 attrs
      pure t
    DiscardedTopOfDeck iid cards (isAbilitySource attrs 1 -> True) (isTarget attrs -> True) -> do
      case cards of
        [] -> pure ()
        (card : _) -> do
          if cdCardType (toCardDef card) == PlayerTreacheryType -- Weakness
            then -- Draw the weakness
              addToHand iid [card]
            else do
              -- Apply modifier for the round based on card type
              let cardType = cdCardType (toCardDef card)
              let modifierType = case cardType of
                    AssetType -> CannotPlay #asset
                    EventType -> CannotPlay #event
                    SkillType -> CannotCommitCards #skill
                    _ -> CannotPlay AnyCard
              createWindowModifierEffect_ EffectRoundWindow attrs iid [modifierType]
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Languor <$> liftRunMessage msg attrs
