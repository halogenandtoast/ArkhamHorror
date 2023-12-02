module Arkham.Treachery.Cards.LostInTheWilds (
  lostInTheWilds,
  LostInTheWilds (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostInTheWilds = LostInTheWilds TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheWilds :: TreacheryCard LostInTheWilds
lostInTheWilds = treachery LostInTheWilds Cards.lostInTheWilds

instance HasModifiersFor LostInTheWilds where
  getModifiersFor (InvestigatorTarget iid) (LostInTheWilds attrs) =
    pure
      $ toModifiers attrs
      $ guard (treacheryOnInvestigator iid attrs)
      *> [CannotMove, CannotExplore]
  getModifiersFor _ _ = pure []

instance HasAbilities LostInTheWilds where
  getAbilities (LostInTheWilds a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnEnds #when You
    ]

instance RunMessage LostInTheWilds where
  runMessage msg t@(LostInTheWilds attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ RevelationSkillTest iid source #willpower 3
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n -> do
      pushAll
        [ assignHorror iid source n
        , attachTreachery attrs iid
        ]
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> LostInTheWilds <$> runMessage msg attrs
