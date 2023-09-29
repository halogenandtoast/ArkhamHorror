module Arkham.Treachery.Cards.Nihilism (
  nihilism,
  Nihilism (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Nihilism = Nihilism TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nihilism :: TreacheryCard Nihilism
nihilism = treachery Nihilism Cards.nihilism

instance HasAbilities Nihilism where
  getAbilities (Nihilism a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ OrWindowMatcher
          [ RevealChaosToken Timing.After You (ChaosTokenFaceIs AutoFail)
          , CancelChaosToken Timing.After You (ChaosTokenFaceIs AutoFail)
          , IgnoreChaosToken Timing.After You (ChaosTokenFaceIs AutoFail)
          ]
    , restrictedAbility a 2 OnSameLocation
        $ ActionAbility Nothing
        $ ActionCost
          2
    ]

instance RunMessage Nihilism where
  runMessage msg t@(Nihilism attrs) = case msg of
    Revelation iid source
      | isSource attrs source ->
          t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          t <$ push (InvestigatorAssignDamage iid source DamageAny 1 1)
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      push $ Discard (toAbilitySource attrs 2) (toTarget attrs)
      pure t
    _ -> Nihilism <$> runMessage msg attrs
