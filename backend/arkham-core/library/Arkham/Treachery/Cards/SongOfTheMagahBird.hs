module Arkham.Treachery.Cards.SongOfTheMagahBird (songOfTheMagahBird, SongOfTheMagahBird (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SongOfTheMagahBird = SongOfTheMagahBird TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheMagahBird :: TreacheryCard SongOfTheMagahBird
songOfTheMagahBird = treachery SongOfTheMagahBird Cards.songOfTheMagahBird

instance HasAbilities SongOfTheMagahBird where
  getAbilities (SongOfTheMagahBird a) =
    [ mkAbility a 1 $ forced $ Leaves #after You $ locationWithTreachery a
    , skillTestAbility $ restrictedAbility a 2 OnSameLocation actionAbility
    ]

instance RunMessage SongOfTheMagahBird where
  runMessage msg t@(SongOfTheMagahBird attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid (push . attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ assignHorror iid (attrs.ability 1) 1
        , toDiscardBy iid (attrs.ability 1) attrs
        , placeDoomOnAgendaAndCheckAdvance
        ]
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      let chooseSkillTest (lbl, sType) = skillTestLabel lbl sType sid iid (attrs.ability 2) attrs (Fixed 4)
      player <- getPlayer iid
      push
        $ chooseOne player
        $ map chooseSkillTest [("Resist the call", #willpower), ("Drive away the birds", #combat)]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> SongOfTheMagahBird <$> runMessage msg attrs
