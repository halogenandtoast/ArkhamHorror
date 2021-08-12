module Arkham.Types.Treachery.Cards.RexsCurse
  ( RexsCurse(..)
  , rexsCurse
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype RexsCurse = RexsCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

rexsCurse :: TreacheryCard RexsCurse
rexsCurse = treachery RexsCurse Cards.rexsCurse

instance HasActions RexsCurse where
  getActions (RexsCurse x) =
    [ restrictedAbility x 1 (InThreatAreaOf You) $ ForcedAbility
        (WouldHaveSkillTestResult
          Timing.When
          You
          AnySkillTest
          (SuccessResult AnyValue)
        )
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
      t <$ pushAll
        (retainedMessages
        <> [ ReturnSkillTestRevealedTokens
           , AddSkillTestSubscriber (TreacheryTarget treacheryId)
           , DrawAnotherToken iid
           ]
        )
    FailedSkillTest iid _ _ (TreacheryTarget tid) _ _ | tid == treacheryId ->
      t <$ push (ShuffleIntoDeck iid (TreacheryTarget treacheryId))
    _ -> RexsCurse <$> runMessage msg attrs
