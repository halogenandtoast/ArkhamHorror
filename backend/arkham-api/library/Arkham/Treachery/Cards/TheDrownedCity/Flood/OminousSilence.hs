module Arkham.Treachery.Cards.TheDrownedCity.Flood.OminousSilence (ominousSilence) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (getFloodLevelFor)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.FloodLevel (FloodLevel (Unflooded))
import Arkham.Matcher
import Arkham.Placement (Placement (InThreatArea))
import Arkham.Treachery.CardDefs.TheDrownedCity.Flood qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OminousSilence = OminousSilence TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ominousSilence :: TreacheryCard OminousSilence
ominousSilence = treachery OminousSilence Cards.ominousSilence

instance HasModifiersFor OminousSilence where
  getModifiersFor (OminousSilence a) = case a.placement of
    InThreatArea iid ->
      modifySelect
        a
        (InvestigatorWithId iid)
        [ AdditionalPlayCostOf
            (basic AnyCard)
            (OrCost [ResourceCost 1, HorrorCost (toSource a) (toTarget iid) 1])
        ]
    _ -> pure ()

instance HasAbilities OminousSilence where
  getAbilities (OminousSilence a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) actionAbility]

instance RunMessage OminousSilence where
  runMessage msg t@(OminousSilence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      floodLevel <- getFloodLevelFor iid
      when (floodLevel == Unflooded) do
        skillTestModifier sid (attrs.ability 1) sid SkillTestAutomaticallySucceeds
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> OminousSilence <$> liftRunMessage msg attrs
