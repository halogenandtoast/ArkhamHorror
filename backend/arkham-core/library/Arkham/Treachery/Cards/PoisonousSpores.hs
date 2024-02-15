module Arkham.Treachery.Cards.PoisonousSpores (
  poisonousSpores,
  PoisonousSpores (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Runner

newtype PoisonousSpores = PoisonousSpores TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poisonousSpores :: TreacheryCard PoisonousSpores
poisonousSpores = treachery PoisonousSpores Cards.poisonousSpores

instance HasAbilities PoisonousSpores where
  getAbilities (PoisonousSpores a) =
    [mkAbility a 1 $ ForcedAbility $ RoundEnds Timing.When]

instance RunMessage PoisonousSpores where
  runMessage msg t@(PoisonousSpores attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid
        $ \lid -> push $ AttachTreachery (toId attrs) (LocationTarget lid)
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      case treacheryPlacement attrs of
        TreacheryAttachedTo (LocationTarget lid) -> do
          takesHorror <-
            select
              $ investigatorAt lid
              <> HasMatchingTreachery
                (treacheryIs Treacheries.poisoned)
          gainsPoisoned <-
            select
              $ investigatorAt lid
              <> NotInvestigator
                (HasMatchingTreachery $ treacheryIs Treacheries.poisoned)
          gainsPoisonedMessages <- for gainsPoisoned $ \iid -> do
            poisoned <- getSetAsidePoisoned
            pure $ CreateWeaknessInThreatArea poisoned iid
          pushAll
            $ [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
              | iid <- takesHorror
              ]
            <> gainsPoisonedMessages
            <> [toDiscard (toAbilitySource attrs 1) attrs]
          pure t
        _ -> error "invalid attachment of treachery, expected location"
    _ -> PoisonousSpores <$> runMessage msg attrs
