module Arkham.Location.Cards.CourtOfTheGreatOldOnesANotTooDistantFuture (
  courtOfTheGreatOldOnesANotTooDistantFuture,
  CourtOfTheGreatOldOnesANotTooDistantFuture (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Effect.Window
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype CourtOfTheGreatOldOnesANotTooDistantFuture
  = CourtOfTheGreatOldOnesANotTooDistantFuture LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheGreatOldOnesANotTooDistantFuture
  :: LocationCard CourtOfTheGreatOldOnesANotTooDistantFuture
courtOfTheGreatOldOnesANotTooDistantFuture =
  location
    CourtOfTheGreatOldOnesANotTooDistantFuture
    Cards.courtOfTheGreatOldOnesANotTooDistantFuture
    3
    (PerPlayer 2)

instance HasAbilities CourtOfTheGreatOldOnesANotTooDistantFuture where
  getAbilities (CourtOfTheGreatOldOnesANotTooDistantFuture a) =
    withRevealedAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ Enters Timing.After You
          $ LocationWithId
          $ toId a
      , haunted "The next action you perform this round must be an investigate action." a 2
      ]

instance RunMessage CourtOfTheGreatOldOnesANotTooDistantFuture where
  runMessage msg l@(CourtOfTheGreatOldOnesANotTooDistantFuture attrs) = case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      CourtOfTheGreatOldOnesANotTooDistantFuture
        <$> runMessage msg (attrs & labelL .~ "courtOfTheGreatOldOnesANotTooDistantFuture")
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid attrs iid SkillWillpower 3
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push
        $ createWindowModifierEffect
          (FirstEffectWindow [EffectRoundWindow, EffectNextActionWindow])
          attrs
          iid
          [MustTakeAction $ IsAction Action.Investigate]
      pure l
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 n
      pure l
    _ -> CourtOfTheGreatOldOnesANotTooDistantFuture <$> runMessage msg attrs
