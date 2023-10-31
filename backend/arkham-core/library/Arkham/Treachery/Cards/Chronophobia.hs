module Arkham.Treachery.Cards.Chronophobia (
  chronophobia,
  Chronophobia (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Chronophobia = Chronophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chronophobia :: TreacheryCard Chronophobia
chronophobia = treachery Chronophobia Cards.chronophobia

instance HasAbilities Chronophobia where
  getAbilities (Chronophobia x) =
    [ restrictedAbility x 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnEnds #when You
    , restrictedAbility x 2 OnSameLocation
        $ ActionAbility Nothing
        $ ActionCost 2
    ]

instance RunMessage Chronophobia where
  runMessage msg t@(Chronophobia attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      t <$ push (InvestigatorDirectDamage iid source 0 1)
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      t <$ push (toDiscardBy iid (toAbilitySource attrs 2) attrs)
    _ -> Chronophobia <$> runMessage msg attrs
