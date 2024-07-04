module Arkham.Treachery.Cards.SecretsInTheAttic (
  secretsInTheAttic,
  SecretsInTheAttic (..),
)
where

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SecretsInTheAttic = SecretsInTheAttic TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsInTheAttic :: TreacheryCard SecretsInTheAttic
secretsInTheAttic = treachery SecretsInTheAttic Cards.secretsInTheAttic

instance HasModifiersFor SecretsInTheAttic where
  getModifiersFor (InvestigatorTarget _) (SecretsInTheAttic a) =
    pure
      $ toModifiers a [CannotTriggerAbilityMatching $ AbilityIsFastAbility <> AbilityOnLocation Anywhere]
  getModifiersFor _ _ = pure []

instance HasAbilities SecretsInTheAttic where
  getAbilities (SecretsInTheAttic a) =
    [ limitedAbility (MaxPer Cards.secretsInTheAttic PerRound 1)
        $ mkAbility a 1
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage SecretsInTheAttic where
  runMessage msg t@(SecretsInTheAttic attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      pushAll
        [ assignHorror iid attrs 1
        , PlaceTreachery (toId attrs) NextToAgenda
        ]
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ toDiscard (attrs.ability 1) attrs
      pure t
    _ -> SecretsInTheAttic <$> runMessage msg attrs
