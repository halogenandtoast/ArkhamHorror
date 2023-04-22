module Arkham.Treachery.Cards.RexsCurse
  ( RexsCurse(..)
  , rexsCurse
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Metadata = Metadata { active :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RexsCurse = RexsCurse (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rexsCurse :: TreacheryCard RexsCurse
rexsCurse = treachery (RexsCurse . (`with` Metadata False)) Cards.rexsCurse

instance HasAbilities RexsCurse where
  getAbilities (RexsCurse (x `With` _)) =
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

instance RunMessage RexsCurse where
  runMessage msg t@(RexsCurse (attrs@TreacheryAttrs {..} `With` metadata)) =
    case msg of
      Revelation iid source | isSource attrs source ->
        t <$ push (AttachTreachery treacheryId (InvestigatorTarget iid))
      UseCardAbility iid source 1 _ _ | isSource attrs source -> do
        retainedMessages <- withQueue $ \queue ->
          let
            (remainingWillPass, queue') = flip span queue $ \case
              Will PassedSkillTest{} -> True
              _ -> False
            (before, after) = flip break queue' $ \case
              Ask iid' (ChooseOne [SkillTestApplyResultsButton]) | iid == iid' -> True
              _ -> False
            remaining = case after of
              [] -> []
              (_ : xs) -> xs
          in (before <> remaining, remainingWillPass)
        pushAll
          $ retainedMessages
          <> [ReturnSkillTestRevealedTokens, DrawAnotherToken iid]
        pure $ RexsCurse (attrs `with` Metadata True)
      FailedSkillTest iid _ _ _ _ _ | treacheryOnInvestigator iid attrs -> do
        when (active metadata) $ push $ ShuffleIntoDeck
          (Deck.InvestigatorDeck iid)
          (toTarget attrs)
        pure $ RexsCurse (attrs `With` Metadata False)
      SkillTestEnds _ _ -> pure $ RexsCurse (attrs `With` Metadata False)
      _ -> RexsCurse . (`with` metadata) <$> runMessage msg attrs
