module Arkham.Treachery.Cards.Languor (languor) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck
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
      [ restricted a 1 (InThreatAreaOf You) $ forced $ TurnBegins #after You
      , restricted a 2 OnSameLocation doubleActionAbility
      ]
    _ -> []

instance RunMessage Languor where
  runMessage msg t@(Languor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatAreaOnlyOne attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfDeckAndHandle iid (attrs.ability 1) 1 attrs
      pure t
    DiscardedTopOfDeck iid (card : _) (isAbilitySource attrs 1 -> True) (isTarget attrs -> True) -> do
      if card.kind == PlayerTreacheryType
        then drawCardFrom iid (InvestigatorDiscard iid) card
        else do
          let mods = case card.kind of
                AssetType -> [CannotPlay #asset, CannotCommitCards #asset]
                EventType -> [CannotPlay #event, CannotCommitCards #event]
                SkillType -> [CannotCommitCards #skill]
                _ -> []
          unless (null mods) do
            createWindowModifierEffect_ EffectRoundWindow attrs iid mods
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Languor <$> liftRunMessage msg attrs
