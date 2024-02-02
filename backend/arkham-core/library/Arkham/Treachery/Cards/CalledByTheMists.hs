module Arkham.Treachery.Cards.CalledByTheMists (
  calledByTheMists,
  CalledByTheMists (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CalledByTheMists = CalledByTheMists TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

calledByTheMists :: TreacheryCard CalledByTheMists
calledByTheMists = treachery CalledByTheMists Cards.calledByTheMists

instance HasAbilities CalledByTheMists where
  getAbilities (CalledByTheMists a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ InitiatedSkillTest Timing.After You AnySkillType (SkillTestGameValue $ AtLeast $ Static 4) #any
    , restrictedAbility a 2 OnSameLocation
        $ ActionAbility []
        $ ActionCost 2
    ]

instance RunMessage CalledByTheMists where
  runMessage msg t@(CalledByTheMists attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      t <$ push (assignDamage iid source 1)
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      t <$ push (toDiscardBy iid (toAbilitySource attrs 2) attrs)
    _ -> CalledByTheMists <$> runMessage msg attrs
