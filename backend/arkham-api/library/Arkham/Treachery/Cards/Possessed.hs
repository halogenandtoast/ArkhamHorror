module Arkham.Treachery.Cards.Possessed (possessed) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (RevealChaosToken)

newtype Possessed = Possessed TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessed :: TreacheryCard Possessed
possessed = treachery Possessed Cards.possessed

instance HasModifiersFor Possessed where
  getModifiersFor (Possessed a) = modifySelf a [IsPointOfHorror]

instance HasAbilities Possessed where
  getAbilities (Possessed a) =
    [ playerLimit PerTestOrAbility
        $ restricted a 1 (InThreatAreaOf You <> DuringSkillTest AnySkillTest)
        $ forced
        $ RevealChaosToken #when You #frost
    ]

instance RunMessage Possessed where
  runMessage msg t@(Possessed attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled "Take 1 horror" $ assignHorror iid (attrs.ability 1) 1
        labeled "Automatically fail this test" failSkillTest
      pure t
    _ -> Possessed <$> liftRunMessage msg attrs
