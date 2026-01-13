module Arkham.Treachery.Cards.DistortedReasoning (distortedReasoning) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (CannotPerformAction), modified_)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DistortedReasoning = DistortedReasoning TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

distortedReasoning :: TreacheryCard DistortedReasoning
distortedReasoning = treachery DistortedReasoning Cards.distortedReasoning

instance HasModifiersFor DistortedReasoning where
  getModifiersFor (DistortedReasoning a) = case a.placement of
    InThreatArea iid -> do
      let as = toResultDefault [] a.meta
      modified_ a iid $ map (CannotPerformAction . IsAction) as
    _ -> pure ()

instance HasAbilities DistortedReasoning where
  getAbilities (DistortedReasoning a) =
    [restricted a 1 InYourThreatArea $ forced $ TurnEnds #when You]

instance RunMessage DistortedReasoning where
  runMessage msg t@(DistortedReasoning attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    TakenActions iid as | attrs.inThreatAreaOf == Just iid -> do
      pure $ DistortedReasoning $ attrs & setMeta as
    EndTurn iid | attrs.inThreatAreaOf == Just iid -> do
      pure $ DistortedReasoning $ attrs & setMeta Null
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DistortedReasoning <$> liftRunMessage msg attrs
