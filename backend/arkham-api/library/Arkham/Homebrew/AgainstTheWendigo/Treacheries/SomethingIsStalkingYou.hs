module Arkham.Homebrew.AgainstTheWendigo.Treacheries.SomethingIsStalkingYou (
  somethingIsStalkingYou,
) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (isSymbolFace)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype SomethingIsStalkingYou = SomethingIsStalkingYou TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingIsStalkingYou :: TreacheryCard SomethingIsStalkingYou
somethingIsStalkingYou = treachery SomethingIsStalkingYou Cards.somethingIsStalkingYou

instance HasModifiersFor SomethingIsStalkingYou where
  getModifiersFor (SomethingIsStalkingYou a) =
    inThreatAreaGets a [SkillModifier #willpower (-1), SkillModifier #intellect (-1)]

instance HasAbilities SomethingIsStalkingYou where
  getAbilities (SomethingIsStalkingYou a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ RoundEnds #when]

instance RunMessage SomethingIsStalkingYou where
  runMessage msg t@(SomethingIsStalkingYou attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) miid tokens -> do
      for_ miid continue_
      unless (any isSymbolFace tokens) $ toDiscard (attrs.ability 1) attrs
      pure t
    _ -> SomethingIsStalkingYou <$> liftRunMessage msg attrs
