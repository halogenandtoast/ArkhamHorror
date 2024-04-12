module Arkham.Treachery.Cards.EvilPast (
  evilPast,
  EvilPast (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EvilPast = EvilPast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evilPast :: TreacheryCard EvilPast
evilPast = treachery EvilPast Cards.evilPast

instance HasAbilities EvilPast where
  getAbilities (EvilPast a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility EncounterDeckRunsOutOfCards
    ]

instance RunMessage EvilPast where
  runMessage msg t@(EvilPast attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasEvilPast <-
        selectAny
          $ treacheryIs Cards.evilPast
          <> Arkham.Matcher.treacheryInThreatAreaOf iid
      push
        $ if hasEvilPast
          then gainSurge attrs
          else attachTreachery attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ assignHorror iid attrs 2
        , beginSkillTest iid (attrs.ability 1) iid #willpower (Fixed 3)
        ]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> EvilPast <$> runMessage msg attrs
