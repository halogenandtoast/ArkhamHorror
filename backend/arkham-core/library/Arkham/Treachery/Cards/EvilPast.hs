module Arkham.Treachery.Cards.EvilPast (
  evilPast,
  EvilPast (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
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
          <> TreacheryInThreatAreaOf
            (InvestigatorWithId iid)
      push
        $ if hasEvilPast
          then gainSurge attrs
          else AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
        , beginSkillTest iid attrs iid SkillWillpower 3
        ]
      pure t
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
        pure t
    _ -> EvilPast <$> runMessage msg attrs
