module Arkham.Treachery.Cards.RexsCurse (RexsCurse (..), rexsCurse) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Metadata = Metadata {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RexsCurse = RexsCurse (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rexsCurse :: TreacheryCard RexsCurse
rexsCurse = treachery (RexsCurse . (`with` Metadata False)) Cards.rexsCurse

instance HasAbilities RexsCurse where
  getAbilities (RexsCurse (x `With` _)) =
    [ playerLimit PerTestOrAbility
        $ restrictedAbility x 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ WouldHaveSkillTestResult #when You AnySkillTest
        $ SuccessResult AnyValue
    ]

instance RunMessage RexsCurse where
  runMessage msg t@(RexsCurse (attrs `With` meta)) =
    case msg of
      Revelation iid (isSource attrs -> True) -> do
        push $ attachTreachery attrs iid
        pure t
      UseThisAbility iid (isSource attrs -> True) 1 -> do
        pushAll [ReturnSkillTestRevealedChaosTokens, DrawAnotherChaosToken iid, RerunSkillTest]
        pure $ RexsCurse (attrs `with` Metadata True)
      FailedSkillTest iid _ _ _ _ _ | treacheryOnInvestigator iid attrs -> do
        pushWhen (active meta)
          $ ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs)
        pure $ RexsCurse (attrs `With` Metadata False)
      SkillTestEnds _ _ -> pure $ RexsCurse (attrs `With` Metadata False)
      _ -> RexsCurse . (`with` meta) <$> runMessage msg attrs
