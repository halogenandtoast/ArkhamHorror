module Arkham.Treachery.Cards.LostInTheWilds
  ( lostInTheWilds
  , LostInTheWilds(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Criteria
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostInTheWilds = LostInTheWilds TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheWilds :: TreacheryCard LostInTheWilds
lostInTheWilds = treachery LostInTheWilds Cards.lostInTheWilds

instance HasModifiersFor LostInTheWilds where
  getModifiersFor (InvestigatorTarget iid) (LostInTheWilds attrs) =
    pure $ if treacheryOnInvestigator iid attrs
      then toModifiers attrs [CannotMove, CannotExplore]
      else []
  getModifiersFor _ _ = pure []

instance HasAbilities LostInTheWilds where
  getAbilities (LostInTheWilds a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ TurnEnds
        Timing.When
        You
    ]

instance RunMessage LostInTheWilds where
  runMessage msg t@(LostInTheWilds attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ RevelationSkillTest iid source SkillWillpower 3
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n -> do
      pushAll
        [ InvestigatorAssignDamage iid source DamageAny n 0
        , AttachTreachery (toId attrs) $ InvestigatorTarget iid
        ]
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> LostInTheWilds <$> runMessage msg attrs
