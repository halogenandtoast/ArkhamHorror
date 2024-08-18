module Arkham.Treachery.Cards.TwistOfFate (TwistOfFate (..), twistOfFate) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TwistOfFate = TwistOfFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistOfFate :: TreacheryCard TwistOfFate
twistOfFate = treachery TwistOfFate Cards.twistOfFate

instance RunMessage TwistOfFate where
  runMessage msg t@(TwistOfFate attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      pure t
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      let
        source = toSource attrs
        (damage, horror) = bimap getSum getSum $ flip foldMap faces \case
          ElderSign -> (Sum 0, Sum 0)
          PlusOne -> (Sum 0, Sum 0)
          Zero -> (Sum 1, Sum 0)
          MinusOne -> (Sum 1, Sum 0)
          MinusTwo -> (Sum 1, Sum 0)
          MinusThree -> (Sum 1, Sum 0)
          MinusFour -> (Sum 1, Sum 0)
          MinusFive -> (Sum 1, Sum 0)
          MinusSix -> (Sum 1, Sum 0)
          MinusSeven -> (Sum 1, Sum 0)
          MinusEight -> (Sum 1, Sum 0)
          Skull -> (Sum 0, Sum 2)
          Cultist -> (Sum 0, Sum 2)
          Tablet -> (Sum 0, Sum 2)
          ElderThing -> (Sum 0, Sum 2)
          AutoFail -> (Sum 0, Sum 2)
          CurseToken -> (Sum 0, Sum 0)
          BlessToken -> (Sum 0, Sum 0)

      pushAll
        $ [InvestigatorAssignDamage iid source DamageAny damage horror | damage > 0 || horror > 0]
        <> [ResetChaosTokens source]
      pure t
    _ -> TwistOfFate <$> runMessage msg attrs
