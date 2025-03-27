module Arkham.Treachery.Cards.Frostbitten (frostbitten) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (RevealChaosToken)

newtype Frostbitten = Frostbitten TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frostbitten :: TreacheryCard Frostbitten
frostbitten = treachery Frostbitten Cards.frostbitten

instance HasModifiersFor Frostbitten where
  getModifiersFor (Frostbitten a) = modifySelf a [IsPointOfDamage]

instance HasAbilities Frostbitten where
  getAbilities (Frostbitten a) =
    [ playerLimit PerTestOrAbility
        $ restricted a 1 (InThreatAreaOf You <> DuringSkillTest AnySkillTest)
        $ forced
        $ RevealChaosToken #when You #frost
    ]

instance RunMessage Frostbitten where
  runMessage msg t@(Frostbitten attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled "Take 1 damage" $ assignDamage iid (attrs.ability 1) 1
        labeled "Automatically fail this test" failSkillTest
      pure t
    _ -> Frostbitten <$> liftRunMessage msg attrs
