module Arkham.Treachery.Cards.ChildrenOfBlood.Afflicted.FrenziedHunger (frenziedHunger) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Matcher
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Afflicted qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FrenziedHunger = FrenziedHunger TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenziedHunger :: TreacheryCard FrenziedHunger
frenziedHunger = treachery FrenziedHunger Cards.frenziedHunger

instance HasModifiersFor FrenziedHunger where
  getModifiersFor (FrenziedHunger attrs) =
    inThreatAreaGets attrs [AdditionalActionCostOf (FirstOneOfPerformed [#draw, #move, #parley]) 1]

instance HasAbilities FrenziedHunger where
  getAbilities (FrenziedHunger a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage FrenziedHunger where
  runMessage msg t@(FrenziedHunger attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasCopy <- selectAny $ treacheryIs Cards.frenziedHunger <> treacheryInThreatAreaOf iid
      if hasCopy then gainSurge attrs else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> FrenziedHunger <$> liftRunMessage msg attrs
