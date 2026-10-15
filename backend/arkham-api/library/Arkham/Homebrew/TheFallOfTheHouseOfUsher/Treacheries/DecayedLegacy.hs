module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Treacheries.DecayedLegacy (decayedLegacy) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype DecayedLegacy = DecayedLegacy TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decayedLegacy :: TreacheryCard DecayedLegacy
decayedLegacy = treachery DecayedLegacy Cards.decayedLegacy

-- Set for the rest of the round by ability 1, cleared by ability 3.
ignoringReduction :: TreacheryAttrs -> Bool
ignoringReduction a = toResultDefault False a.meta

instance HasModifiersFor DecayedLegacy where
  -- "You get -2 to each of your skills."
  getModifiersFor (DecayedLegacy a) =
    inThreatAreaGets a [AnySkillValue (-2) | not (ignoringReduction a)]

instance HasAbilities DecayedLegacy where
  getAbilities (DecayedLegacy a) =
    [ restricted a 1 (InThreatAreaOf You) $ FastAbility $ DoomCost (a.ability 1) (toTarget a) 1
    , restricted a 2 OnSameLocation doubleActionAbility
    ]
      <> [mkAbility a 3 $ silent $ RoundEnds #when | ignoringReduction a]

instance RunMessage DecayedLegacy where
  runMessage msg t@(DecayedLegacy attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> pure . DecayedLegacy $ attrs & metaL .~ toJSON True
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 3 -> pure . DecayedLegacy $ attrs & metaL .~ toJSON False
    _ -> DecayedLegacy <$> liftRunMessage msg attrs
