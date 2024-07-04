module Arkham.Treachery.Cards.VoiceOfTheJungle (voiceOfTheJungle, VoiceOfTheJungle (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VoiceOfTheJungle = VoiceOfTheJungle TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voiceOfTheJungle :: TreacheryCard VoiceOfTheJungle
voiceOfTheJungle = treachery VoiceOfTheJungle Cards.voiceOfTheJungle

instance HasAbilities VoiceOfTheJungle where
  getAbilities (VoiceOfTheJungle x) =
    [ restrictedAbility
        x
        1
        (InThreatAreaOf You <> youExist NoSuccessfulExploreThisTurn)
        $ forced
        $ TurnEnds #at You
    , restrictedAbility x 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage VoiceOfTheJungle where
  runMessage msg t@(VoiceOfTheJungle attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      beginSkillTest iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> VoiceOfTheJungle <$> liftRunMessage msg attrs
