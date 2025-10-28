module Arkham.Treachery.Cards.SongOfTheMagahBird (songOfTheMagahBird) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SongOfTheMagahBird = SongOfTheMagahBird TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheMagahBird :: TreacheryCard SongOfTheMagahBird
songOfTheMagahBird = treachery SongOfTheMagahBird Cards.songOfTheMagahBird

instance HasAbilities SongOfTheMagahBird where
  getAbilities (SongOfTheMagahBird a) =
    [ mkAbility a 1 $ forced $ Leaves #after You $ locationWithTreachery a
    , skillTestAbility $ restricted a 2 OnSameLocation actionAbility
    ]

instance RunMessage SongOfTheMagahBird where
  runMessage msg t@(SongOfTheMagahBird attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      toDiscardBy iid (attrs.ability 1) attrs
      placeDoomOnAgendaAndCheckAdvance 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      let chooseSkillTest lbl sType = labeled lbl $ beginSkillTest sid iid (attrs.ability 2) attrs sType (Fixed 4)
      chooseOneM iid do
        chooseSkillTest "Resist the call" #willpower
        chooseSkillTest "Drive away the birds" #combat
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> SongOfTheMagahBird <$> liftRunMessage msg attrs
