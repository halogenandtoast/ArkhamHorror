module Arkham.Treachery.Cards.DisquietingDreams (
  disquietingDreams,
  DisquietingDreams (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DisquietingDreams = DisquietingDreams TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disquietingDreams :: TreacheryCard DisquietingDreams
disquietingDreams = treachery DisquietingDreams Cards.disquietingDreams

instance HasAbilities DisquietingDreams where
  getAbilities (DisquietingDreams a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $
        ForcedAbility $
          TurnEnds Timing.AtIf You
    , restrictedAbility a 2 (InThreatAreaOf You) $
        ForcedAbility $
          EncounterDeckRunsOutOfCards
    ]

instance RunMessage DisquietingDreams where
  runMessage msg t@(DisquietingDreams attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs SkillWillpower 5
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ DiscardTopOfEncounterDeck iid 1 (toSource attrs) Nothing
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      pushAll
        [ Discard (toSource attrs) (toTarget attrs)
        , Search
            iid
            (toSource attrs)
            (toTarget iid)
            [(FromTopOfDeck 10, DiscardRest)]
            WeaknessCard
            (DrawFound iid 10)
        ]
      pure t
    _ -> DisquietingDreams <$> runMessage msg attrs
