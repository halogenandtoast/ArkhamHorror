module Arkham.Treachery.Cards.RexsCurse
  ( RexsCurse(..)
  , rexsCurse
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype RexsCurse = RexsCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

rexsCurse :: TreacheryCard RexsCurse
rexsCurse = treachery RexsCurse Cards.rexsCurse

instance HasAbilities RexsCurse where
  getAbilities (RexsCurse x) =
    [ restrictedAbility
        x
        1
        (InThreatAreaOf You)
        (ForcedAbility
        $ WouldHaveSkillTestResult Timing.When You AnySkillTest
        $ SuccessResult AnyValue
        )
      & abilityLimitL
      .~ PlayerLimit PerTestOrAbility 1
    ]

instance TreacheryRunner env => RunMessage env RexsCurse where
  runMessage msg t@(RexsCurse attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId (InvestigatorTarget iid))
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      retainedMessages <- withQueue $ \queue ->
        let
          (remainingWillPass, queue') = flip span queue $ \case
            Will PassedSkillTest{} -> True
            _ -> False
          (before, after) = flip break queue' $ \case
            Ask iid' (ChooseOne [SkillTestApplyResults]) | iid == iid' -> True
            _ -> False
          remaining = case after of
            [] -> []
            (_ : xs) -> xs
        in (before <> remaining, remainingWillPass)
      pushAll
        $ retainedMessages
        <> [ CreateEffect
             (toCardCode attrs)
             Nothing
             (toSource attrs)
             (toTarget attrs)
           , ReturnSkillTestRevealedTokens
           , DrawAnotherToken iid
           ]
      pure t
    FailedSkillTest iid _ _ (TreacheryTarget tid) _ _ | tid == treacheryId ->
      t <$ push (ShuffleIntoDeck iid (TreacheryTarget treacheryId))
    _ -> RexsCurse <$> runMessage msg attrs
