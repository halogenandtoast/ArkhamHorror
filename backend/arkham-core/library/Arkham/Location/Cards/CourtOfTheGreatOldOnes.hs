module Arkham.Location.Cards.CourtOfTheGreatOldOnes (
  courtOfTheGreatOldOnes,
  CourtOfTheGreatOldOnes (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Effect.Window
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype CourtOfTheGreatOldOnes = CourtOfTheGreatOldOnes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheGreatOldOnes :: LocationCard CourtOfTheGreatOldOnes
courtOfTheGreatOldOnes =
  location CourtOfTheGreatOldOnes Cards.courtOfTheGreatOldOnes 3 (PerPlayer 2)

instance HasAbilities CourtOfTheGreatOldOnes where
  getAbilities (CourtOfTheGreatOldOnes a) = withRevealedAbilities a
    [ mkAbility a 1
      $ ForcedAbility
      $ Enters Timing.After You
      $ LocationWithId
      $ toId a
    , haunted "The next action you perform this round must be an investigate action." a 2
    ]

instance RunMessage CourtOfTheGreatOldOnes where
  runMessage msg l@(CourtOfTheGreatOldOnes attrs) = case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      CourtOfTheGreatOldOnes
        <$> runMessage msg (attrs & labelL .~ "courtOfTheGreatOldOnes")
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid attrs iid SkillWillpower 3
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ createWindowModifierEffect (FirstEffectWindow [EffectRoundWindow, EffectNextActionWindow]) attrs iid [MustTakeAction $ IsAction Action.Investigate]
      pure l
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ n -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 n
      pure l
    _ -> CourtOfTheGreatOldOnes <$> runMessage msg attrs
