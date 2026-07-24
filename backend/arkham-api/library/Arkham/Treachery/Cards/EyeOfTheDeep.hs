module Arkham.Treachery.Cards.EyeOfTheDeep (eyeOfTheDeep) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Placement (Placement (InThreatArea))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EyeOfTheDeep = EyeOfTheDeep TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfTheDeep :: TreacheryCard EyeOfTheDeep
eyeOfTheDeep = treachery EyeOfTheDeep Cards.eyeOfTheDeep

instance HasModifiersFor EyeOfTheDeep where
  getModifiersFor (EyeOfTheDeep a) = case a.placement of
    InThreatArea iid ->
      modified_
        a
        iid
        [ ForcedChaosTokenChange #cultist [#elderthing]
        , ForcedChaosTokenChange #tablet [#elderthing]
        ]
    _ -> pure ()

instance HasAbilities EyeOfTheDeep where
  getAbilities (EyeOfTheDeep a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) actionAbility]

instance RunMessage EyeOfTheDeep where
  runMessage msg t@(EyeOfTheDeep attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      other <- selectAny $ TreacheryInThreatAreaOf (be iid) <> treacheryIs Cards.eyeOfTheDeep
      if other then toDiscard attrs attrs else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> EyeOfTheDeep <$> liftRunMessage msg attrs
